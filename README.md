# g711
test g711 convertion

We can conclude that the result encode by function 'linear2ulaw' is differnt from by function 'ulaw_compress' which is published
in ITU software.

So we made a little change.

Code before:

unsigned char linear2ulaw( short pcm_val)	/* 2's complement (16-bit range) */
{linear2ulaw
	short		mask;
	short		seg;
	unsigned char		uval;

	/* Get the sign and the magnitude of the value. */
	pcm_val = pcm_val >> 2;
	if (pcm_val < 0) {
		pcm_val = -pcm_val;
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
Code after:

unsigned char linear2ulaw( short pcm_val)	/* 2's complement (16-bit range) */
{linear2ulaw
	short		mask;
	short		seg;
	unsigned char		uval;

	/* Get the sign and the magnitude of the value. */
	pcm_val = pcm_val >> 2;
	if (pcm_val < 0) {
		pcm_val = -pcm_val -1 ; // modify by DumBoFly
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
