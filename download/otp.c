/* otp.c - One-time pad implementation
 *
 * Copyright (c) 2008 Christopher Wellons <mosquitopsu@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for
 * any purpose with or without fee is hereby granted, provided that
 * the above copyright notice and this permission notice appear in all
 * copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
 * AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* The first argument is the one-time pad. The one-time pad is copied
 * to the (optional) second argument as it is read in, which may be
 * handy if /dev/random is used as the pad (which isn't really useful
 * at all).
 */
#include <stdio.h>
#include <stdlib.h>

#define BSIZE 2048   /* Buffer size. */

int main (int argc, char **argv)
{
  if (argc < 2 || argc > 3)
    {
      fprintf (stderr, "Usage: %s OTP_IN [OPT_OUT]\n", argv[0]);
      return 1;
    }
  
  /* Open the one-time pad files. */
  FILE * otp = fopen (argv[1], "rb");
  if (otp == NULL)
    {
      fprintf (stderr, "Unable to open the one-time pad. Quitting.\n");
      return 1;
    }
  FILE * otp_out = NULL;
  if (argc == 3)
    {
      otp_out = fopen (argv[2], "wb");
      if (otp_out == NULL)
	{
	  fprintf (stderr, "Unable to open the one-time pad output. "
		   "Quitting.\n");
	  return 1;
	}
    }
  
  /* Encrypt stdin in BSIZE-byte-sized chunks at a time. */
  unsigned char p_buffer[BSIZE], k_buffer[BSIZE];
  while (!feof(stdin))
    {
      size_t p_in = fread (p_buffer, 1, BSIZE, stdin);
      if (p_in == 0)
	break;
      size_t k_in = fread (k_buffer, 1, p_in, otp);
      if (k_in != p_in)
	{
	  fprintf (stderr, "The one-time pad is too short. Quitting.\n");
	  break;
	}
      
      /* XOR the buffers */
      size_t i;
      for (i = 0; i < p_in; i++)
	p_buffer[i] ^= k_buffer[i];
      
      /* Write out the buffers. */
      fwrite (&p_buffer[0], 1, p_in, stdout);
      if (argc == 3)
	fwrite (&k_buffer[0], 1, p_in, otp_out);
    }
  
  /* Clean up */
  fclose (otp);
  if (argc == 3)
    fclose (otp_out);
  return 0;
}
