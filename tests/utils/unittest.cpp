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

#include "utils.h"		/* API for misc_math functions */

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

// Test fixture for utils_norm_angle()
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

// Test fixture for utils_norm_angle_rad()
class NormAngle_rad : public MiscMath {
protected:
  virtual void SetUp() {
  }

  virtual void TearDown() {
  }
protected:
   float eps = 0.000001f;

};

TEST_F(NormAngle_rad, ValInRange) {
   float inputVal;

   inputVal = 0.0;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(M_PI, inputVal, eps);

   inputVal = M_PI-1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(M_PI-1, inputVal, eps);

   inputVal = -M_PI+1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(-M_PI+1, inputVal, eps);

   inputVal = -1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(-1, inputVal);
}

TEST_F(NormAngle_rad, ValBelowRange) {
   float inputVal;

   inputVal = -0.0;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(0, inputVal);

   inputVal = -M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_EQ(-M_PI, inputVal);

   inputVal = -2*M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(0, inputVal, eps);

   inputVal = -2*M_PI-1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(-1, inputVal, eps);

   inputVal = -3*M_PI+1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(-M_PI+1, inputVal, eps);
}


TEST_F(NormAngle_rad, ValAboveRange) {
   float inputVal;

   inputVal = 2*M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(0, inputVal, eps);

   inputVal = 2*M_PI+1;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(1, inputVal, eps);

   inputVal = 3.5*M_PI;
   utils_norm_angle_rad(&inputVal);
   EXPECT_NEAR(-0.5*M_PI, inputVal, eps);
}

