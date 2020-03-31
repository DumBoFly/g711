/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use.  Users may copy or modify this source code without
 * charge.
 *
 * SUN SOURCE CODE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING
 * THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS SOFTWARE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * g711.c
 *
 * u-law, A-law and linear PCM conversions.
 */

/*
 * December 30, 1994:
 * Functions linear2alaw, linear2ulaw have been updated to correctly
 * convert unquantized 16 bit values.
 * Tables for direct u- to A-law and A- to u-law conversions have been
 * corrected.
 * Borge Lindberg, Center for PersonKommunikation, Aalborg University.
 * bli@cpk.auc.dk
 *
 */
 
#define	SIGN_BIT	(0x80)		/* Sign bit for a A-law byte. */
#define	QUANT_MASK	(0xf)		/* Quantization field mask. */
#define	NSEGS		(8)		/* Number of A-law segments. */
#define	SEG_SHIFT	(4)		/* Left shift for segment number. */
#define	SEG_MASK	(0x70)		/* Segment field mask. */

static int seg_aend[8] = {0x1F, 0x3F, 0x7F, 0xFF,
			    0x1FF, 0x3FF, 0x7FF, 0xFFF};
static int seg_uend[8] = {0x3F, 0x7F, 0xFF, 0x1FF,
			    0x3FF, 0x7FF, 0xFFF, 0x1FFF};

/* copy from CCITT G.711 specifications */
unsigned char u2a[128] = {			/* u- to A-law conversions */
	1,	1,	2,	2,	3,	3,	4,	4,
	5,	5,	6,	6,	7,	7,	8,	8,
	9,	10,	11,	12,	13,	14,	15,	16,
	17,	18,	19,	20,	21,	22,	23,	24,
	25,	27,	29,	31,	33,	34,	35,	36,
	37,	38,	39,	40,	41,	42,	43,	44,
	46,	48,	49,	50,	51,	52,	53,	54,
	55,	56,	57,	58,	59,	60,	61,	62,
	64,	65,	66,	67,	68,	69,	70,	71,
	72,	73,	74,	75,	76,	77,	78,	79,
/* corrected:
	81,	82,	83,	84,	85,	86,	87,	88, 
   should be: */
	80,	82,	83,	84,	85,	86,	87,	88,
	89,	90,	91,	92,	93,	94,	95,	96,
	97,	98,	99,	100,	101,	102,	103,	104,
	105,	106,	107,	108,	109,	110,	111,	112,
	113,	114,	115,	116,	117,	118,	119,	120,
	121,	122,	123,	124,	125,	126,	127,	128};

unsigned char a2u[128] = {			/* A- to u-law conversions */
	1,	3,	5,	7,	9,	11,	13,	15,
	16,	17,	18,	19,	20,	21,	22,	23,
	24,	25,	26,	27,	28,	29,	30,	31,
	32,	32,	33,	33,	34,	34,	35,	35,
	36,	37,	38,	39,	40,	41,	42,	43,
	44,	45,	46,	47,	48,	48,	49,	49,
	50,	51,	52,	53,	54,	55,	56,	57,
	58,	59,	60,	61,	62,	63,	64,	64,
	65,	66,	67,	68,	69,	70,	71,	72,
/* corrected:
	73,	74,	75,	76,	77,	78,	79,	79,
   should be: */
	73,	74,	75,	76,	77,	78,	79,	80,

	80,	81,	82,	83,	84,	85,	86,	87,
	88,	89,	90,	91,	92,	93,	94,	95,
	96,	97,	98,	99,	100,	101,	102,	103,
	104,	105,	106,	107,	108,	109,	110,	111,
	112,	113,	114,	115,	116,	117,	118,	119,
	120,	121,	122,	123,	124,	125,	126,	127};

static int
search(
	int		val,	/* changed from "short" *drago* */
	int *	table,
	int		size)	/* changed from "short" *drago* */
{
	int		i;		/* changed from "short" *drago* */

	for (i = 0; i < size; i++) {
		if (val <= *table++)
			return (i);
	}
	return (size);
}

/*
 * linear2alaw() - Convert a 16-bit linear PCM value to 8-bit A-law
 *
 * linear2alaw() accepts an 16-bit integer and encodes it as A-law data.
 *
 *		Linear Input Code	Compressed Code
 *	------------------------	---------------
 *	0000000wxyza			000wxyz
 *	0000001wxyza			001wxyz
 *	000001wxyzab			010wxyz
 *	00001wxyzabc			011wxyz
 *	0001wxyzabcd			100wxyz
 *	001wxyzabcde			101wxyz
 *	01wxyzabcdef			110wxyz
 *	1wxyzabcdefg			111wxyz
 *
 * For further information see John C. Bellamy's Digital Telephony, 1982,
 * John Wiley & Sons, pps 98-111 and 472-476.
 */
unsigned char linear2alaw(short	pcm_val)        /* 2's complement (16-bit range) */
                                        /* changed from "short" *drago* */
{
	short		mask;	/* changed from "short" *drago* */
	short		seg;	/* changed from "short" *drago* */
	unsigned char		aval;

	pcm_val = pcm_val >> 3;

	if (pcm_val >= 0) {
		mask = 0xD5;		/* sign (7th) bit = 1 */
	} else {
		mask = 0x55;		/* sign bit = 0 */
		pcm_val = -pcm_val - 1;
	}

	/* Convert the scaled magnitude to segment number. */
	seg = search(pcm_val, seg_aend, 8);

	/* Combine the sign, segment, and quantization bits. */

	if (seg >= 8)		/* out of range, return maximum value. */
		return (0x7F ^ mask);
	else {
		aval = seg << SEG_SHIFT;
		if (seg < 2)
			aval |= (pcm_val >> 1) & QUANT_MASK;
		else
			aval |= (pcm_val >> seg) & QUANT_MASK;
		return (aval ^ mask);
	}
}

/*
 * alaw2linear() - Convert an A-law value to 16-bit linear PCM
 *
 */
short alaw2linear(unsigned char	a_val)		
{
	short		t;      /* changed from "short" *drago* */
	short		seg;    /* changed from "short" *drago* */

	a_val ^= 0x55;

	t = (a_val & QUANT_MASK) << 4;
	seg = ((unsigned)a_val & SEG_MASK) >> SEG_SHIFT;
	switch (seg) {
	case 0:
		t += 8;
		break;
	case 1:
		t += 0x108;
		break;
	default:
		t += 0x108;
		t <<= seg - 1;
	}
	return ((a_val & SIGN_BIT) ? t : -t);
}

#define	BIAS		(0x84)		/* Bias for linear code. */
#define CLIP            8159

/*
 * linear2ulaw() - Convert a linear PCM value to u-law
 *
 * In order to simplify the encoding process, the original linear magnitude
 * is biased by adding 33 which shifts the encoding range from (0 - 8158) to
 * (33 - 8191). The result can be seen in the following encoding table:
 *
 *	Biased Linear Input Code	Compressed Code
 *	------------------------	---------------
 *	00000001wxyza			000wxyz
 *	0000001wxyzab			001wxyz
 *	000001wxyzabc			010wxyz
 *	00001wxyzabcd			011wxyz
 *	0001wxyzabcde			100wxyz
 *	001wxyzabcdef			101wxyz
 *	01wxyzabcdefg			110wxyz
 *	1wxyzabcdefgh			111wxyz
 *
 * Each biased linear code has a leading 1 which identifies the segment
 * number. The value of the segment number is equal to 7 minus the number
 * of leading 0's. The quantization interval is directly available as the
 * four bits wxyz.  * The trailing bits (a - h) are ignored.
 *
 * Ordinarily the complement of the resulting code word is used for
 * transmission, and so the code word is complemented before it is returned.
 *
 * For further information see John C. Bellamy's Digital Telephony, 1982,
 * John Wiley & Sons, pps 98-111 and 472-476.
 */
unsigned char linear2ulaw( short pcm_val)	/* 2's complement (16-bit range) */
{
	short		mask;
	short		seg;
	unsigned char		uval;

	/* Get the sign and the magnitude of the value. */
	pcm_val = pcm_val >> 2;
	if (pcm_val < 0) {
		pcm_val = -pcm_val -1 ; // modify by wanglong : as 
		mask = 0x7F;
	} else {
		mask = 0xFF;
	}
        if ( pcm_val > CLIP ) pcm_val = CLIP;		/* clip the magnitude */
	pcm_val += (BIAS >> 2);

	/* Convert the scaled magnitude to segment number. */
	seg = search(pcm_val, seg_uend, 8);

	/*
	 * Combine the sign, segment, quantization bits;
	 * and complement the code word.
	 */
	if (seg >= 8)		/* out of range, return maximum value. */
		return (0x7F ^ mask);
	else {
		uval = (seg << 4) | ((pcm_val >> (seg + 1)) & 0xF);
		return (uval ^ mask);
	}

}

/*
 * ulaw2linear() - Convert a u-law value to 16-bit linear PCM
 *
 * First, a biased linear code is derived from the code word. An unbiased
 * output can then be obtained by subtracting 33 from the biased code.
 *
 * Note that this function expects to be passed the complement of the
 * original code word. This is in keeping with ISDN conventions.
 */
short ulaw2linear( unsigned char u_val)
{
	short t;

	/* Complement to obtain normal u-law value. */
	u_val = ~u_val;

	/*
	 * Extract and bias the quantization bits. Then
	 * shift up by the segment number and subtract out the bias.
	 */
	t = ((u_val & QUANT_MASK) << 3) + BIAS;
	t <<= (u_val & SEG_MASK) >> SEG_SHIFT;

	return ((u_val & SIGN_BIT) ? (BIAS - t) : (t - BIAS));
}

/* A-law to u-law conversion */
static int alaw2ulaw (int	aval)
{
	aval &= 0xff;
	return ((aval & 0x80) ? (0xFF ^ a2u[aval ^ 0xD5]) :
	    (0x7F ^ a2u[aval ^ 0x55]));
}

/* u-law to A-law conversion */
static int ulaw2alaw (int	uval)
{
	uval &= 0xff;
	return ((uval & 0x80) ? (0xD5 ^ (u2a[0xFF ^ uval] - 1)) :
	    (0x55 ^ (u2a[0x7F ^ uval] - 1)));
}

unsigned char alaw_compress (short	pcm_val) {
	short ix, iexp;
	unsigned char res;

	ix = pcm_val < 0          /* 0 <= ix < 2048 */
      ? (~pcm_val) >> 4       /* 1's complement for negative values */
      : (pcm_val) >> 4;

    /* Do more, if exponent > 0 */
    if (ix > 15) {              /* exponent=0 for ix <= 15 */
      iexp = 1;                 /* first step: */
      while (ix > 16 + 15) {    /* find mantissa and exponent */
        ix >>= 1;
        iexp++;
      }
      ix -= 16;                 /* second step: remove leading '1' */

      ix += iexp << 4;          /* now compute encoded value */
    }
    if (pcm_val >= 0)
      ix |= (0x0080);           /* add sign bit */

    res = ix ^ (0x0055);  /* toggle even bits */
    return res;
}


short alaw_expand (unsigned char pcm_val) {
	short ix, mant, iexp;
	short res;


    ix = pcm_val ^ (0x0055);  /* re-toggle toggled bits */

    ix &= (0x007F);             /* remove sign bit */
    iexp = ix >> 4;             /* extract exponent */
    mant = ix & (0x000F);       /* now get mantissa */
    if (iexp > 0)
      mant = mant + 16;         /* add leading '1', if exponent > 0 */

    mant = (mant << 4) + (0x0008);      /* now mantissa left justified and */
    /* 1/2 quantization step added */
    if (iexp > 1)               /* now left shift according exponent */
      mant = mant << (iexp - 1);

    res = pcm_val > 127 /* invert, if negative sample */
      ? mant : -mant;
    return res;

}

unsigned char ulaw_compress (short pcm_val) {
	long n;                       /* samples's count */
	short i;                      /* aux.var. */
	short absno;                  /* absolute value of linear (input) sample */
	short segno;                  /* segment (Table 2/G711, column 1) */
	short low_nibble;             /* low nibble of log companded sample */
	short high_nibble;            /* high nibble of log companded sample */
	unsigned char res;
	short tem;

    /* -------------------------------------------------------------------- */
    /* Change from 14 bit left justified to 14 bit right justified */
    /* Compute absolute value; adjust for easy processing */
    /* -------------------------------------------------------------------- */
    if (pcm_val < 0)
    	tem = ~pcm_val;
    absno = pcm_val < 0       /* compute 1's complement in case of */
      ? ((~pcm_val) >> 2) + 33        /* negative samples */
      : ((pcm_val) >> 2) + 33;        /* NB: 33 is the difference value */

    /* between the thresholds for */
    /* A-law and u-law. */
    if (absno > (0x1FFF))       /* limitation to "absno" < 8192 */
      absno = (0x1FFF);

    /* Determination of sample's segment */
    i = absno >> 6;
    segno = 1;
    while (i != 0) {
      segno++;
      i >>= 1;
    }

    /* Mounting the high-nibble of the log-PCM sample */
    high_nibble = (0x0008) - segno;

    /* Mounting the low-nibble of the log PCM sample */
    low_nibble = (absno >> segno)       /* right shift of mantissa and */
      &(0x000F);                /* masking away leading '1' */
    low_nibble = (0x000F) - low_nibble;

    /* Joining the high-nibble and the low-nibble of the log PCM sample */
    res = (high_nibble << 4) | low_nibble;

    /* Add sign bit */
    if (pcm_val >= 0)
      res = res | (0x0080);
    return res;
}


short ulaw_expand (unsigned char pcm_value) {
	long n;                       /* aux.var. */
	short segment;                /* segment (Table 2/G711, column 1) */
	short mantissa;               /* low nibble of log companded sample */
	short exponent;               /* high nibble of log companded sample */
	short sign;                   /* sign of output sample */
	short step;
	short res;


    sign = pcm_value < (0x0080) /* sign-bit = 1 for positiv values */
      ? -1 : 1;
    mantissa = ~pcm_value;      /* 1's complement of input value */
    exponent = (mantissa >> 4) & (0x0007);      /* extract exponent */
    segment = exponent + 1;     /* compute segment number */
    mantissa = mantissa & (0x000F);     /* extract mantissa */

    /* Compute Quantized Sample (14 bit left justified!) */
    step = (4) << segment;      /* position of the LSB */
    /* = 1 quantization step) */
    res = sign *          /* sign */
      (((0x0080) << exponent)   /* '1', preceding the mantissa */
       +step * mantissa         /* left shift of mantissa */
       + step / 2               /* 1/2 quantization step */
       - 4 * 33);
	
	return res;
}


int main() {
	int i;
	int res, ares;
#if 1
	for(i = -32767; i<32768; i++) {
		res = linear2alaw(i);
		ares = alaw_compress(i);
		if (res != ares)
			printf("linear to a law input value %d output %x  %x\n", i, res, ares);
	}
	printf("linear to a law  done\n");
	for(i = 0; i<256; i++) {
		res = alaw2linear(i);
		ares = alaw_expand(i);
		if (res != ares)
			printf("a law to linear input value %d output %x  %x\n", i, (short)res, ares);
	}
	printf("a law to linear done\n");
#endif
#if 1
	for(i = -32767; i<32768;  i++) {
		res = linear2ulaw(i);
		ares = ulaw_compress(i);
		if (res != ares)
			printf("linear to u law input value %d output %x  %x\n", i, res, ares);

	}
	printf("linear to u law done\n");
	for(i = 0; i<256; i++) {
		res = ulaw2linear(i);
		ares = ulaw_expand(i);
		if (res != ares)
			printf("u law to linear input value %d output %x  %x\n", i, res, ares);
	}
	printf("u law to linear done\n");
#endif
	return 0;
}