// Standalone sanity check: calls the onnx2c-generated moons_entry()
// directly (no LispBM involved) and compares against the ground-truth
// logits/classes dumped by train_moons.py into test_cases.json.
//
// Not part of the LispBM build - a one-off cross-check tool. Build with:
//   gcc -O2 -lm moons_gen.c test_native.c -o test_native

#include <stdio.h>
#include <math.h>

void moons_entry(const float tensor_input[1][2], float tensor_output[1][2]);

typedef struct {
  float x, y;
  float expect_logit0, expect_logit1;
  int expect_class;
} test_case_t;

// Mirrors test_cases.json (kept in sync by hand for this one-off check).
static const test_case_t cases[] = {
  { -1.0f,  0.4f,  6.748284339904785f, -6.9443511962890625f, 0 },
  {  0.0f,  1.0f,  1.555171251296997f, -2.4521560668945312f, 0 },
  {  1.0f, -0.4f, -2.50641131401062f,   2.2265877723693848f, 1 },
  {  2.0f,  0.4f, -0.8457441329956055f, -0.9335192441940308f, 0 },
  {  0.5f, -0.3f, -2.03623366355896f,   2.0250840187072754f, 1 },
};

int main(void) {
  int failures = 0;
  for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
    const test_case_t *c = &cases[i];
    float in[1][2]  = {{ c->x, c->y }};
    float out[1][2] = {{ 0.0f, 0.0f }};

    moons_entry(in, out);

    int pred = out[0][1] > out[0][0] ? 1 : 0;
    float d0 = fabsf(out[0][0] - c->expect_logit0);
    float d1 = fabsf(out[0][1] - c->expect_logit1);

    printf("(%.2f, %.2f) -> logits (%.6f, %.6f) class %d | expected (%.6f, %.6f) class %d | diff (%.6f, %.6f)\n",
           c->x, c->y, out[0][0], out[0][1], pred,
           c->expect_logit0, c->expect_logit1, c->expect_class,
           d0, d1);

    if (pred != c->expect_class || d0 > 1e-3f || d1 > 1e-3f) {
      failures++;
    }
  }

  if (failures == 0) {
    printf("SUCCESS\n");
    return 0;
  } else {
    printf("FAILURE (%d mismatches)\n", failures);
    return 1;
  }
}
