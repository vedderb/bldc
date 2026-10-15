#!/usr/bin/env python3
"""Trains a tiny MLP on sklearn's make_moons and exports it to ONNX.

This is a toy end-to-end test case for LispBM's tinyml extensions, not a
real-world model. make_moons produces two interleaving, non-linearly
separable crescents, so the network is forced to actually use its hidden
layer + ReLU rather than solving the task linearly.

Pipeline: this script -> moons.onnx -> `onnx2c moons.onnx -fmoons_entry
-o moons_gen.c` -> a hand-written adapter implementing tinyml_model_t
around the generated moons_entry() function.
"""

import json

import torch
import torch.nn as nn
from sklearn.datasets import make_moons

torch.manual_seed(0)

INPUT_DIM = 2
HIDDEN_DIM = 8
OUTPUT_DIM = 2


class MoonsNet(nn.Module):
    def __init__(self):
        super().__init__()
        self.fc1 = nn.Linear(INPUT_DIM, HIDDEN_DIM)
        self.relu = nn.ReLU()
        self.fc2 = nn.Linear(HIDDEN_DIM, OUTPUT_DIM)

    def forward(self, x):
        return self.fc2(self.relu(self.fc1(x)))


def main():
    X, y = make_moons(n_samples=500, noise=0.15, random_state=0)
    X = torch.tensor(X, dtype=torch.float32)
    y = torch.tensor(y, dtype=torch.long)

    model = MoonsNet()
    optimizer = torch.optim.Adam(model.parameters(), lr=0.05)
    loss_fn = nn.CrossEntropyLoss()

    for epoch in range(200):
        optimizer.zero_grad()
        logits = model(X)
        loss = loss_fn(logits, y)
        loss.backward()
        optimizer.step()

    with torch.no_grad():
        preds = model(X).argmax(dim=1)
        acc = (preds == y).float().mean().item()
    print(f"final loss: {loss.item():.4f}  train accuracy: {acc:.4f}")

    model.eval()

    # Export with a fixed batch size of 1 - onnx2c needs concrete shapes,
    # and tinyml_extensions.c has no concept of a batch dimension anyway.
    example_input = torch.zeros(1, INPUT_DIM, dtype=torch.float32)
    torch.onnx.export(
        model,
        example_input,
        "moons.onnx",
        input_names=["input"],
        output_names=["output"],
        opset_version=13,
        dynamic_axes=None,
        dynamo=False,
    )
    print("wrote moons.onnx")

    # A handful of test points, picked clearly inside one crescent or the
    # other (not near the decision boundary), with their expected class
    # and raw logits - ground truth for cross-checking onnx2c's native
    # output and, later, tinyml-run's output, against PyTorch.
    test_points = [
        (-1.0, 0.4),
        (0.0, 1.0),
        (1.0, -0.4),
        (2.0, 0.4),
        (0.5, -0.3),
    ]
    with torch.no_grad():
        test_x = torch.tensor(test_points, dtype=torch.float32)
        test_logits = model(test_x)
        test_preds = test_logits.argmax(dim=1)

    cases = []
    for point, logits, pred in zip(test_points, test_logits.tolist(), test_preds.tolist()):
        cases.append({
            "input": list(point),
            "logits": logits,
            "class": pred,
        })

    with open("test_cases.json", "w") as f:
        json.dump(cases, f, indent=2)
    print("wrote test_cases.json")
    for c in cases:
        print(c)


if __name__ == "__main__":
    main()
