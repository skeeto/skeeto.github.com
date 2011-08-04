/* Copyright (c) 2008 Christopher Wellons <mosquitopsu@gmail.com>
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
#include <stdio.h>

#define TAB_MAX 1000000
short tab[TAB_MAX];

int get_val (unsigned int x)
{
  short val;
  
  if (x >= TAB_MAX || tab[x - 1] == 0)
    {
      unsigned int xn;
      if (x & 1)
	xn = x * 3 + 1;
      else
	xn = x / 2;

      val = 1 + get_val (xn);
      if (x < TAB_MAX)
	tab[x - 1] = val;
    }
  else
    val = tab[x - 1];
  
  return val;
}

int max_seq (int a, int b)
{
  if (b < a)
    {
      int swap = a;
      a = b;
      b = swap;
    }
  
  int max = 0;
  unsigned int i;
  for (i = a; i <= b; i++)
    {
      int val = get_val (i);
      if (val > max)
	max = val;
    }
  
  return max;
}

int main ()
{
  tab[0] = 1;
  
  int a, b;
  while (!feof (stdin))
    {
      int ret = scanf ("%d %d", &a, &b);
      if (ret == EOF)
	break;
      printf ("%d %d %d\n", a, b, max_seq (a, b));
    }

  return 0;
}
