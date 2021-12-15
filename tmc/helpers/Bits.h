// BIT DEFINITION
#ifndef TMC_BITS_H_
#define TMC_BITS_H_

#define  BIT0   0x00000001
#define  BIT1   0x00000002
#define  BIT2   0x00000004
#define  BIT3   0x00000008
#define  BIT4   0x00000010
#define  BIT5   0x00000020
#define  BIT6   0x00000040
#define  BIT7   0x00000080
#define  BIT8   0x00000100
#define  BIT9   0x00000200
#define  BIT10  0x00000400
#define  BIT11  0x00000800
#define  BIT12  0x00001000
#define  BIT13  0x00002000
#define  BIT14  0x00004000
#define  BIT15  0x00008000
#define  BIT16  0x00010000
#define  BIT17  0x00020000
#define  BIT18  0x00040000
#define  BIT19  0x00080000
#define  BIT20  0x00100000
#define  BIT21  0x00200000
#define  BIT22  0x00400000
#define  BIT23  0x00800000
#define  BIT24  0x01000000
#define  BIT25  0x02000000
#define  BIT26  0x04000000
#define  BIT27  0x08000000
#define  BIT28  0x10000000
#define  BIT29  0x20000000
#define  BIT30  0x40000000
#define  BIT31  0x80000000

#define BYTE0_MASK   0x00000000000000FF
#define BYTE0_SHIFT  0
#define BYTE1_MASK   0x000000000000FF00
#define BYTE1_SHIFT  8
#define BYTE2_MASK   0x0000000000FF0000
#define BYTE2_SHIFT  16
#define BYTE3_MASK   0x00000000FF000000
#define BYTE3_SHIFT  24
#define BYTE4_MASK   0x000000FF00000000
#define BYTE4_SHIFT  32
#define BYTE5_MASK   0x0000FF0000000000
#define BYTE5_SHIFT  40
#define BYTE6_MASK   0x00FF000000000000
#define BYTE6_SHIFT  48
#define BYTE7_MASK   0xFF00000000000000
#define BYTE7_SHIFT  56

#define SHORT0_MASK   (BYTE0_MASK|BYTE1_MASK)
#define SHORT0_SHIFT  BYTE0_SHIFT
#define SHORT1_MASK   (BYTE2_MASK|BYTE3_MASK)
#define SHORT1_SHIFT  BYTE2_SHIFT
#define SHORT2_MASK   (BYTE4_MASK|BYTE5_MASK)
#define SHORT2_SHIFT  BYTE4_SHIFT
#define SHORT3_MASK   (BYTE6_MASK|BYTE7_MASK)
#define SHORT3_SHIFT  BYTE6_SHIFT

#define WORD0_MASK    (SHORT0_MASK|SHORT1_MASK)
#define WORD0_SHIFT   SHORT0_SHIFT
#define WORD1_MASK    (SHORT2_MASK|SHORT3_MASK)
#define WORD1_SHIFT   SHORT2_SHIFT

#define NIBBLE(value, n)  (((value) >> ((n) << 2)) & 0x0F)
#define BYTE(value, n)    (((value) >> ((n) << 3)) & 0xFF)
#define SHORT(value, n)   (((value) >> ((n) << 4)) & 0xFFFF)
#define WORD(value, n)    (((value) >> ((n) << 5)) & 0xFFFFFFFF)

#define _8_16(__1, __0)                                (((__1) << BYTE1_SHIFT) | ((__0) << BYTE0_SHIFT))

#define _8_32(__3, __2, __1, __0)                      (((__3) << BYTE3_SHIFT) | ((__2) << BYTE2_SHIFT) | ((__1) << BYTE1_SHIFT) | ((__0) << BYTE0_SHIFT))
#define _16_32(__1, __0)                               (((__1) << SHORT1_SHIFT) | ((__0) << SHORT0_SHIFT))

#define _8_64(__7, __6, __5, __4, __3, __2, __1, __0)  (((__7) << BYTE7_SHIFT) | ((__6) << BYTE6_SHIFT) | ((__5) << BYTE5_SHIFT) | ((__4) << BYTE4_SHIFT) | ((__3) << BYTE3_SHIFT) | ((__2) << BYTE2_SHIFT) | ((__1) << BYTE1_SHIFT) | ((__0) << BYTE0_SHIFT))
#define _16_64(__3, __2, __1, __0)                     (((__3) << SHORT3_SHIFT) | ((__2) << SHORT2_SHIFT) | ((__1) << SHORT1_SHIFT) | ((__0) << SHORT0_SHIFT))
#define _32_64(__1, __0)                               (((__1) << WORD1_SHIFT) | ((__0) << WORD0_SHIFT))

#define SIGN_EXTEND(base, bit, extended) ((extended) ((base) | ((((extended)(base)) & (1 << (bit))) << ((sizeof(extended) * 8) - (bit) - 1))))

#endif /* TMC_BITS_H_ */
