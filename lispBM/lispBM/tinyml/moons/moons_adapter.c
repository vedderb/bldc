// tinyml_model_t adapter wrapping the onnx2c-generated moons_entry().
//
// Regenerate moons_gen.c with:
//   onnx2c -f moons_entry moons.onnx > moons_gen.c

#include <extensions/tinyml_extensions.h>

void moons_entry(const float tensor_input[1][2], float tensor_output[1][2]);

static int moons_run(const tinyml_model_t *m, const void *const *in, void *const *out) {
  (void)m;
  moons_entry((const float (*)[2])in[0], (float (*)[2])out[0]);
  return 0;
}

static const tinyml_tensor_info_t moons_in  = { .dtype = TINYML_F32, .ndim = 1, .shape = {2}, .scale = 1.0f };
static const tinyml_tensor_info_t moons_out = { .dtype = TINYML_F32, .ndim = 1, .shape = {2}, .scale = 1.0f };

static const tinyml_model_t moons_model = {
  .name = "moons",
  .n_inputs = 1, .inputs = &moons_in,
  .n_outputs = 1, .outputs = &moons_out,
  .run = moons_run,
};

void moons_model_register(void) {
  lbm_tinyml_register(&moons_model);
}
