/**
 * @file FusionTypes.h
 * @author Seb Madgwick
 * @brief Common types and their associated operations.
 *
 * Static inline implementations are used to optimise for increased execution
 * speed.
 */

#ifndef FUSION_TYPES_H
#define FUSION_TYPES_H

//------------------------------------------------------------------------------
// Includes

#include <math.h> // M_PI, sqrtf, atan2f, asinf
#include <stdint.h> // int32_t

//------------------------------------------------------------------------------
// Definitions

/**
 * @brief Three-dimensional spacial vector.
 */
typedef union {
    float array[3];

    struct {
        float x;
        float y;
        float z;
    } axis;
} FusionVector3;

/**
 * @brief Quaternion.  This library uses the conversion of placing the 'w'
 * element as the first element.  Other implementations may place the 'w'
 * element as the last element.
 */
typedef union {
    float array[4];

    struct {
        float w;
        float x;
        float y;
        float z;
    } element;
} FusionQuaternion;

/**
 * @brief Rotation matrix in row-major order.
 * See http://en.wikipedia.org/wiki/Row-major_order
 */
typedef union {
    float array[9];

    struct {
        float xx;
        float xy;
        float xz;
        float yx;
        float yy;
        float yz;
        float zx;
        float zy;
        float zz;
    } element;
} FusionRotationMatrix;

/**
 * @brief Euler angles union.  The Euler angles are in the Aerospace sequence
 * also known as the ZYX sequence.
 */
typedef union {
    float array[3];

    struct {
        float roll;
        float pitch;
        float yaw;
    } angle;
} FusionEulerAngles;

/**
 * @brief Zero-length vector definition.
 */
#define FUSION_VECTOR3_ZERO ((FusionVector3){ .array = {0.0f, 0.0f, 0.0f} })

/**
 * @brief Quaternion identity definition to represent an aligned of
 * orientation.
 */
#define FUSION_QUATERNION_IDENTITY ((FusionQuaternion){ .array = {1.0f, 0.0f, 0.0f, 0.0f} })

/**
 * @brief Rotation matrix identity definition to represent an aligned of
 * orientation.
 */
#define FUSION_ROTATION_MATRIX_IDENTITY ((FusionRotationMatrix){ .array = {1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f} })

/**
 * @brief Euler angles zero definition to represent an aligned of orientation.
 */
#define FUSION_EULER_ANGLES_ZERO ((FusionEulerAngles){ .array = {0.0f, 0.0f, 0.0f} })

/**
 * @brief Definition of M_PI.  Some compilers may not define this in math.h.
 */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

//------------------------------------------------------------------------------
// Inline functions - Degrees and radians conversion

/**
 * @brief Converts degrees to radians.
 * @param degrees Degrees.
 * @return Radians.
 */
static inline __attribute__((always_inline)) float FusionDegreesToRadians(const float degrees) {
    return degrees * ((float) M_PI / 180.0f);
}

/**
 * @brief Converts radians to degrees.
 * @param radians Radians.
 * @return Degrees.
 */
static inline __attribute__((always_inline)) float FusionRadiansToDegrees(const float radians) {
    return radians * (180.0f / (float) M_PI);
}

//------------------------------------------------------------------------------
// Inline functions - Fast inverse square root

/**
 * @brief Calculates the reciprocal of the square root.
 * See http://en.wikipedia.org/wiki/Fast_inverse_square_root
 * @param x Operand.
 * @return Reciprocal of the square root of x.
 */
static inline __attribute__((always_inline)) float FusionFastInverseSqrt(const float x) {
	if (x == 0.0) {
		return 1.0;
	}
    return 1.0 / sqrtf(x);
}

//------------------------------------------------------------------------------
// Inline functions - Vector operations

/**
 * @brief Adds two vectors.
 * @param vectorA First vector of the operation.
 * @param vectorB Second vector of the operation.
 * @return Sum of vectorA and vectorB.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorAdd(const FusionVector3 vectorA, const FusionVector3 vectorB) {
    FusionVector3 result;
    result.axis.x = vectorA.axis.x + vectorB.axis.x;
    result.axis.y = vectorA.axis.y + vectorB.axis.y;
    result.axis.z = vectorA.axis.z + vectorB.axis.z;
    return result;
}

/**
 * @brief Subtracts two vectors.
 * @param vectorA First vector of the operation.
 * @param vectorB Second vector of the operation.
 * @return vectorB subtracted from vectorA.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorSubtract(const FusionVector3 vectorA, const FusionVector3 vectorB) {
    FusionVector3 result;
    result.axis.x = vectorA.axis.x - vectorB.axis.x;
    result.axis.y = vectorA.axis.y - vectorB.axis.y;
    result.axis.z = vectorA.axis.z - vectorB.axis.z;
    return result;
}

/**
 * @brief Multiplies vector by a scalar.
 * @param vector Vector to be multiplied.
 * @param scalar Scalar to be multiplied.
 * @return Vector multiplied by scalar.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorMultiplyScalar(const FusionVector3 vector, const float scalar) {
    FusionVector3 result;
    result.axis.x = vector.axis.x * scalar;
    result.axis.y = vector.axis.y * scalar;
    result.axis.z = vector.axis.z * scalar;
    return result;
}

/**
 * @brief Calculates the Hadamard product (element-wise multiplication) of two
 * vectors.
 * @param vectorA First vector of the operation.
 * @param vectorB Second vector of the operation.
 * @return Hadamard product of vectorA and vectorB.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorHadamardProduct(const FusionVector3 vectorA, const FusionVector3 vectorB) {
    FusionVector3 result;
    result.axis.x = vectorA.axis.x * vectorB.axis.x;
    result.axis.y = vectorA.axis.y * vectorB.axis.y;
    result.axis.z = vectorA.axis.z * vectorB.axis.z;
    return result;
}

/**
 * @brief Calculates the cross-product of two vectors.
 * @param vectorA First vector of the operation.
 * @param vectorB Second vector of the operation.
 * @return Cross-product of vectorA and vectorB.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorCrossProduct(const FusionVector3 vectorA, const FusionVector3 vectorB) {
#define A vectorA.axis // define shorthand labels for more readable code
#define B vectorB.axis
    FusionVector3 result;
    result.axis.x = A.y * B.z - A.z * B.y;
    result.axis.y = A.z * B.x - A.x * B.z;
    result.axis.z = A.x * B.y - A.y * B.x;
    return result;
#undef A // undefine shorthand labels
#undef B
}

/**
 * @brief Calculates the vector magnitude squared.
 * @param vector Vector of the operation.
 * @return Vector magnitude squared.
 */
static inline __attribute__((always_inline)) float FusionVectorMagnitudeSquared(const FusionVector3 vector) {
#define V vector.axis // define shorthand label for more readable code
    return V.x * V.x + V.y * V.y + V.z * V.z;
#undef V // undefine shorthand label
}

/**
 * @brief Calculates the magnitude of a vector.
 * @param vector Vector to be used in calculation.
 * @return Vector magnitude.
 */
static inline __attribute__((always_inline)) float FusionVectorMagnitude(const FusionVector3 vector) {
    return sqrtf(FusionVectorMagnitudeSquared(vector));
}

/**
 * @brief Normalises a vector to be of unit magnitude.
 * @param vector Vector to be normalised.
 * @return Normalised vector.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorNormalise(const FusionVector3 vector) {
    const float magnitudeReciprocal = 1.0f / sqrtf(FusionVectorMagnitudeSquared(vector));
    return FusionVectorMultiplyScalar(vector, magnitudeReciprocal);
}

/**
 * @brief Normalises a vector to be of unit magnitude using the fast inverse
 * square root approximation.
 * @param vector Vector to be normalised.
 * @return Normalised vector.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionVectorFastNormalise(const FusionVector3 vector) {
    const float magnitudeReciprocal = FusionFastInverseSqrt(FusionVectorMagnitudeSquared(vector));
    return FusionVectorMultiplyScalar(vector, magnitudeReciprocal);
}

//------------------------------------------------------------------------------
// Inline functions - Quaternion operations

/**
 * @brief Adds two quaternions.
 * @param quaternionA First quaternion of the operation.
 * @param quaternionB Second quaternion of the operation.
 * @return Sum of quaternionA and quaternionB.
 */
static inline __attribute__((always_inline)) FusionQuaternion FusionQuaternionAdd(const FusionQuaternion quaternionA, const FusionQuaternion quaternionB) {
    FusionQuaternion result;
    result.element.w = quaternionA.element.w + quaternionB.element.w;
    result.element.x = quaternionA.element.x + quaternionB.element.x;
    result.element.y = quaternionA.element.y + quaternionB.element.y;
    result.element.z = quaternionA.element.z + quaternionB.element.z;
    return result;
}

/**
 * @brief Multiplies two quaternions.
 * @param quaternionA First quaternion of the operation.
 * @param quaternionB Second quaternion of the operation.
 * @return quaternionA multiplied by quaternionB.
 */
static inline __attribute__((always_inline)) FusionQuaternion FusionQuaternionMultiply(const FusionQuaternion quaternionA, const FusionQuaternion quaternionB) {
#define A quaternionA.element // define shorthand labels for more readable code
#define B quaternionB.element
    FusionQuaternion result;
    result.element.w = A.w * B.w - A.x * B.x - A.y * B.y - A.z * B.z;
    result.element.x = A.w * B.x + A.x * B.w + A.y * B.z - A.z * B.y;
    result.element.y = A.w * B.y - A.x * B.z + A.y * B.w + A.z * B.x;
    result.element.z = A.w * B.z + A.x * B.y - A.y * B.x + A.z * B.w;
    return result;
#undef A // undefine shorthand labels
#undef B
}

/**
 * @brief Multiplies quaternion by a vector.  This is a normal quaternion
 * multiplication where the vector is treated a quaternion with a 'w' element
 * value of 0.  The quaternion is post multiplied by the vector.
 * @param quaternion Quaternion to be multiplied.
 * @param vector Vector to be multiplied.
 * @return Quaternion multiplied by vector.
 */
static inline __attribute__((always_inline)) FusionQuaternion FusionQuaternionMultiplyVector(const FusionQuaternion quaternion, const FusionVector3 vector) {
#define Q quaternion.element // define shorthand labels for more readable code
#define V vector.axis
    FusionQuaternion result;
    result.element.w = -Q.x * V.x - Q.y * V.y - Q.z * V.z;
    result.element.x = Q.w * V.x + Q.y * V.z - Q.z * V.y;
    result.element.y = Q.w * V.y - Q.x * V.z + Q.z * V.x;
    result.element.z = Q.w * V.z + Q.x * V.y - Q.y * V.x;
    return result;
#undef Q // undefine shorthand labels
#undef V
}

/**
 * @brief Returns the quaternion conjugate.
 * @param quaternion Quaternion to be conjugated.
 * @return Conjugated quaternion.
 */
static inline __attribute__((always_inline)) FusionQuaternion FusionQuaternionConjugate(const FusionQuaternion quaternion) {
    FusionQuaternion conjugate;
    conjugate.element.w = quaternion.element.w;
    conjugate.element.x = -1.0f * quaternion.element.x;
    conjugate.element.y = -1.0f * quaternion.element.y;
    conjugate.element.z = -1.0f * quaternion.element.z;
    return conjugate;
}

/**
 * @brief Normalises a quaternion to be of unit magnitude.
 * @param quaternion Quaternion to be normalised.
 * @return Normalised quaternion.
 */
static inline __attribute__((always_inline)) FusionQuaternion FusionQuaternionNormalise(const FusionQuaternion quaternion) {
#define Q quaternion.element // define shorthand label for more readable code
    const float magnitudeReciprocal = 1.0f / sqrtf(Q.w * Q.w + Q.x * Q.x + Q.y * Q.y + Q.z * Q.z);
    FusionQuaternion normalisedQuaternion;
    normalisedQuaternion.element.w = Q.w * magnitudeReciprocal;
    normalisedQuaternion.element.x = Q.x * magnitudeReciprocal;
    normalisedQuaternion.element.y = Q.y * magnitudeReciprocal;
    normalisedQuaternion.element.z = Q.z * magnitudeReciprocal;
    return normalisedQuaternion;
#undef Q // undefine shorthand label
}

/**
 * @brief Normalises a quaternion to be of unit magnitude using the fast inverse
 * square root approximation.
 * @param quaternion Quaternion to be normalised.
 * @return Normalised quaternion.
 */
static inline __attribute__((always_inline)) FusionQuaternion FusionQuaternionFastNormalise(const FusionQuaternion quaternion) {
#define Q quaternion.element // define shorthand label for more readable code
    const float magnitudeReciprocal = FusionFastInverseSqrt(Q.w * Q.w + Q.x * Q.x + Q.y * Q.y + Q.z * Q.z);
    FusionQuaternion normalisedQuaternion;
    normalisedQuaternion.element.w = Q.w * magnitudeReciprocal;
    normalisedQuaternion.element.x = Q.x * magnitudeReciprocal;
    normalisedQuaternion.element.y = Q.y * magnitudeReciprocal;
    normalisedQuaternion.element.z = Q.z * magnitudeReciprocal;
    return normalisedQuaternion;
#undef Q // undefine shorthand label
}

//------------------------------------------------------------------------------
// Inline functions - Rotation matrix operations

/**
 * @brief Multiplies two rotation matrices.
 * @param rotationMatrixA First rotation matrix of the operation.
 * @param rotationMatrixB Second rotation matrix of the operation.
 * @return rotationMatrixA with rotationMatrixB.
 */
static inline __attribute__((always_inline)) FusionRotationMatrix FusionRotationMatrixMultiply(const FusionRotationMatrix rotationMatrixA, const FusionRotationMatrix rotationMatrixB) {
#define A rotationMatrixA.element // define shorthand label for more readable code
#define B rotationMatrixB.element
    FusionRotationMatrix result;
    result.element.xx = A.xx * B.xx + A.xy * B.yx + A.xz * B.zx;
    result.element.xy = A.xx * B.xy + A.xy * B.yy + A.xz * B.zy;
    result.element.xz = A.xx * B.xz + A.xy * B.yz + A.xz * B.zz;
    result.element.yx = A.yx * B.xx + A.yy * B.yx + A.yz * B.zx;
    result.element.yy = A.yx * B.xy + A.yy * B.yy + A.yz * B.zy;
    result.element.yz = A.yx * B.xz + A.yy * B.yz + A.yz * B.zz;
    result.element.zx = A.zx * B.xx + A.zy * B.yx + A.zz * B.zx;
    result.element.zy = A.zx * B.xy + A.zy * B.yy + A.zz * B.zy;
    result.element.zz = A.zx * B.xz + A.zy * B.yz + A.zz * B.zz;
    return result;
#undef A // undefine shorthand label
#undef B
}

/**
 * @brief Multiplies rotation matrix with scalar.
 * @param rotationMatrix Rotation matrix to be multiplied.
 * @param vector Vector to be multiplied.
 * @return Rotation matrix multiplied with scalar.
 */
static inline __attribute__((always_inline)) FusionVector3 FusionRotationMatrixMultiplyVector(const FusionRotationMatrix rotationMatrix, const FusionVector3 vector) {
#define R rotationMatrix.element // define shorthand label for more readable code
    FusionVector3 result;
    result.axis.x = R.xx * vector.axis.x + R.xy * vector.axis.y + R.xz * vector.axis.z;
    result.axis.y = R.yx * vector.axis.x + R.yy * vector.axis.y + R.yz * vector.axis.z;
    result.axis.z = R.zx * vector.axis.x + R.zy * vector.axis.y + R.zz * vector.axis.z;
    return result;
#undef R // undefine shorthand label
}

//------------------------------------------------------------------------------
// Inline functions - Conversion operations

/**
 * @brief Converts a quaternion to a rotation matrix.
 * @param quaternion Quaternion to be converted.
 * @return Rotation matrix.
 */
static inline __attribute__((always_inline)) FusionRotationMatrix FusionQuaternionToRotationMatrix(const FusionQuaternion quaternion) {
#define Q quaternion.element // define shorthand label for more readable code
    const float qwqw = Q.w * Q.w; // calculate common terms to avoid repeated operations
    const float qwqx = Q.w * Q.x;
    const float qwqy = Q.w * Q.y;
    const float qwqz = Q.w * Q.z;
    const float qxqy = Q.x * Q.y;
    const float qxqz = Q.x * Q.z;
    const float qyqz = Q.y * Q.z;
    FusionRotationMatrix rotationMatrix;
    rotationMatrix.element.xx = 2.0f * (qwqw - 0.5f + Q.x * Q.x);
    rotationMatrix.element.xy = 2.0f * (qxqy + qwqz);
    rotationMatrix.element.xz = 2.0f * (qxqz - qwqy);
    rotationMatrix.element.yx = 2.0f * (qxqy - qwqz);
    rotationMatrix.element.yy = 2.0f * (qwqw - 0.5f + Q.y * Q.y);
    rotationMatrix.element.yz = 2.0f * (qyqz + qwqx);
    rotationMatrix.element.zx = 2.0f * (qxqz + qwqy);
    rotationMatrix.element.zy = 2.0f * (qyqz - qwqx);
    rotationMatrix.element.zz = 2.0f * (qwqw - 0.5f + Q.z * Q.z);
    return rotationMatrix;
#undef Q // undefine shorthand label
}

/**
 * @brief Converts a quaternion to Euler angles in degrees.
 * @param quaternion Quaternion to be converted.
 * @return Euler angles in degrees.
 */
static inline __attribute__((always_inline)) FusionEulerAngles FusionQuaternionToEulerAngles(const FusionQuaternion quaternion) {
#define Q quaternion.element // define shorthand label for more readable code
    const float qwqwMinusHalf = Q.w * Q.w - 0.5f; // calculate common terms to avoid repeated operations
    FusionEulerAngles eulerAngles;
    eulerAngles.angle.roll = FusionRadiansToDegrees(atan2f(Q.y * Q.z - Q.w * Q.x, qwqwMinusHalf + Q.z * Q.z));
    eulerAngles.angle.pitch = FusionRadiansToDegrees(-1.0f * asinf(2.0f * (Q.x * Q.z + Q.w * Q.y)));
    eulerAngles.angle.yaw = FusionRadiansToDegrees(atan2f(Q.x * Q.y - Q.w * Q.z, qwqwMinusHalf + Q.x * Q.x));
    return eulerAngles;
#undef Q // undefine shorthand label
}

#endif

//------------------------------------------------------------------------------
// End of file
