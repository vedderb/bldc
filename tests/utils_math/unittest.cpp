/*
 * NOTE: This program uses the Google Test infrastructure to drive the unit test
 *
 * Main site for Google Test: https://github.com/google/googletest
 * Documentation and examples: http://google.github.io/googletest/
 */

#include "gtest/gtest.h"

#include <stdio.h>		/* printf */
#include <stdlib.h>		/* abort */
#include <string.h>		/* memset */
#include <stdint.h>		/* uint*_t */

extern "C" {

#include "utils_math.h"		/* API for misc_math functions */

}

#include <math.h>		/* fabs() */

// To use a test fixture, derive a class from testing::Test.
class MiscMath : public testing::Test {
protected:
  virtual void SetUp() {
  }

  virtual void TearDown() {
  }
};


//----------------------------------------
// Test fixture for utils_norm_angle()
//----------------------------------------
class NormAngle_deg : public MiscMath {
protected:
  virtual void SetUp() {
  }

  virtual void TearDown() {
  }
};

TEST_F(NormAngle_deg, ValInRange) {
   float inputVal;

   inputVal = 0.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = 10.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(10, inputVal);

   inputVal = 350.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(350, inputVal);
}

TEST_F(NormAngle_deg, ValBelowRange) {
   float inputVal;

   inputVal = -0.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = -360.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = -10.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(350, inputVal);

   inputVal = -370.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(350, inputVal);
}


TEST_F(NormAngle_deg, ValAboveRange) {
   float inputVal;

   inputVal = 360.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = 370.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(10, inputVal);

   inputVal = 1079.0;
   utils_norm_angle(&inputVal);
   EXPECT_EQ(359, inputVal);
}


//----------------------------------------
// Test fixture for utils_norm_angle_rad()
//----------------------------------------
class NormAngle_rad : public MiscMath {
protected:
  virtual void SetUp() {
  }

  virtual void TearDown() {
  }
protected:
   float eps = 0.000001f;

};


/**
 * @brief TEST_F Test input values which are *in* the range of the function output
 */
TEST_F(NormAngle_rad, ValInRange) {
   float inputVal;

   inputVal = 0.0;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = -0.0;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = -M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(-M_PI, inputVal);

   inputVal = M_PI-1e-6;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(M_PI-1e-6, inputVal);

   inputVal = M_PI-1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(M_PI-1, inputVal);

   inputVal = -M_PI+1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(-M_PI+1, inputVal);

   inputVal = -1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(-1, inputVal);
}


/**
 * @brief TEST_F Test input values which are *below* the range of the function output
 */
TEST_F(NormAngle_rad, ValBelowRange) {
   float inputVal;

   inputVal = -M_PI-1e-6;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(M_PI-1e-6, inputVal);

   inputVal = -2*M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(0, inputVal);

   inputVal = -2*M_PI-1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(-1, inputVal);

   inputVal = -3*M_PI-1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(M_PI-1, inputVal);
}


/**
 * @brief TEST_F Test input values which are *above* the range of the function output
 */
TEST_F(NormAngle_rad, ValAboveRange) {
   float inputVal;

   inputVal = M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(-M_PI, inputVal);

   inputVal = 2*M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(0, inputVal);

   inputVal = 2*M_PI+1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(1, inputVal);

   inputVal = 3.5*M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_FLOAT_EQ(-0.5*M_PI, inputVal);
}


//----------------------------------------
// Test fixture for utils_saturate_vector_2d()
//----------------------------------------
class Saturate2dVector : public MiscMath {
protected:
  virtual void SetUp() {
  }

  virtual void TearDown() {
  }
protected:
   float eps = 0.000001f;

};

TEST_F(Saturate2dVector, ValsInRange) {
   float inputVal_x;
   float inputVal_y;
   float inputVal_max;
   bool ret;

   inputVal_x = 1.0;
   inputVal_y = 1.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(1, inputVal_x);
   EXPECT_EQ(1, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = 1.0;
   inputVal_y = -1.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(1, inputVal_x);
   EXPECT_EQ(-1, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = -1.0;
   inputVal_y = 1.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(-1, inputVal_x);
   EXPECT_EQ(1, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = -1.0;
   inputVal_y = -1.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(-1, inputVal_x);
   EXPECT_EQ(-1, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = 0.01;
   inputVal_y = 0.01;
   inputVal_max = 0.1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(0.01, inputVal_x);
   EXPECT_EQ(0.01, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = 0.01;
   inputVal_y = -0.01;
   inputVal_max = 0.1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(0.01, inputVal_x);
   EXPECT_EQ(-0.01, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = -0.01;
   inputVal_y = 0.01;
   inputVal_max = 0.1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(-0.01, inputVal_x);
   EXPECT_EQ(0.01, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = -0.01;
   inputVal_y = -0.01;
   inputVal_max = 0.1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(-0.01, inputVal_x);
   EXPECT_EQ(-0.01, inputVal_y);
   EXPECT_EQ(false, ret);
}


TEST_F(Saturate2dVector, ValsOnEdgeOfRange) {
   float inputVal_x;
   float inputVal_y;
   float inputVal_max;
   bool ret;

   inputVal_x = 1.0;
   inputVal_y = 1.0;
   inputVal_max = sqrtf(2);
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(1, inputVal_x);
   EXPECT_FLOAT_EQ(1, inputVal_y);
   /* EXPECT_EQ(false, ret); <-- Do not test, since this is on the edge and we don't need to know if it's explicitly inside or outside*/

   inputVal_x = .1;
   inputVal_y = .1;
   inputVal_max = sqrt(.2);
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(.1, inputVal_x);
   EXPECT_FLOAT_EQ(.1, inputVal_y);
   /* EXPECT_EQ(false, ret); <-- Do not test, since this is on the edge and we don't need to know if it's explicitly inside or outside*/
}


TEST_F(Saturate2dVector, ValsBeyondRange) {
   float inputVal_x;
   float inputVal_y;
   float inputVal_max;
   bool ret;

   inputVal_x = 10.0;
   inputVal_y = 10.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(10/sqrt(2), inputVal_x);
   EXPECT_FLOAT_EQ(10/sqrt(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = -10.0;
   inputVal_y = -10.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(-10/sqrt(2), inputVal_x);
   EXPECT_FLOAT_EQ(-10/sqrt(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = 10.0;
   inputVal_y = -10.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(10/sqrt(2), inputVal_x);
   EXPECT_FLOAT_EQ(-10/sqrt(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = -10.0;
   inputVal_y = 10.0;
   inputVal_max = 10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(-10/sqrtf(2), inputVal_x);
   EXPECT_FLOAT_EQ(10/sqrtf(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = .1;
   inputVal_y = .1;
   inputVal_max = .1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(.10/sqrtf(2), inputVal_x);
   EXPECT_FLOAT_EQ(.10 / sqrtf(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = .1;
   inputVal_y = -.1;
   inputVal_max = .1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(.10f/sqrtf(2), inputVal_x);
   EXPECT_FLOAT_EQ(-.10f/sqrtf(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = -.1;
   inputVal_y = .1;
   inputVal_max = .1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(-.10f/sqrtf(2), inputVal_x);
   EXPECT_FLOAT_EQ(.10f/sqrtf(2), inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = -.1;
   inputVal_y = -.1;
   inputVal_max = .1;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(-.10f/sqrtf(2), inputVal_x);
   EXPECT_FLOAT_EQ(-.10f/sqrtf(2), inputVal_y);
   EXPECT_EQ(true, ret);
}

TEST_F(Saturate2dVector, ValsAreDegenerate) {
   float inputVal_x;
   float inputVal_y;
   float inputVal_max;
   bool ret;

   inputVal_x = 0.0;
   inputVal_y = 0.0;
   inputVal_max = 0.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(0, inputVal_x);
   EXPECT_EQ(0, inputVal_y);
   EXPECT_EQ(true, ret);

   inputVal_x = 1.0;
   inputVal_y = 1.0;
   inputVal_max = -10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_EQ(1, inputVal_x);
   EXPECT_EQ(1, inputVal_y);
   EXPECT_EQ(false, ret);

   inputVal_x = 10.0;
   inputVal_y = 10.0;
   inputVal_max = -10.0;
   ret = utils_saturate_vector_2d(&inputVal_x, &inputVal_y, inputVal_max);
   EXPECT_FLOAT_EQ(10/sqrtf(2), inputVal_x);
   EXPECT_FLOAT_EQ(10/sqrtf(2), inputVal_y);
   EXPECT_EQ(true, ret);
}
