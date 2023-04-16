/*
  Arbitrary Precision uLisp Extension - Version 2 - 16th April 2023
  See http://www.ulisp.com/show?282B
*/

#define MAX_VAL ((uint64_t)0xFFFFFFFF)
#define int_to_bignum(x) (cons(number(x), NULL))
enum { SMALLER = -1, EQUAL = 0, LARGER = 1 };

// Internal utility functions

/*
  maybe_gc - Does a garbage collection if less than 1/16 workspace remains.
*/
void maybe_gc(object *arg, object *env) {
  if (Freespace <= WORKSPACESIZE>>4) gc(arg, env);
}

/*
  checkbignum - checks argument is cons.
  It makes the other routines simpler if we don't allow a null list.
*/
object *checkbignum (object *b) {
  if (!consp(b)) error(PSTR("argument is not a bignum"), b);
  return b;
}

/*
  bignum_zerop - Tests whether a bignum is zero, allowing for possible trailing zeros.
*/
bool bignum_zerop (object *bignum) {
  while (bignum != NULL) {
    if (checkinteger(car(bignum)) != 0) return false;
    bignum = cdr(bignum);
  }
  return true;
}

/*
  bignum_normalise - Destructively removes trailing zeros.
*/
object *bignum_normalise (object *bignum) {
  object *result = bignum;
  object *last = bignum;
  while (bignum != NULL) {
    if (checkinteger(car(bignum)) != 0) last = bignum;
    bignum = cdr(bignum);
  }
  cdr(last) = NULL;
  return result;
}

/*
  copylist - Returns a copy of a list.
*/
object *copylist (object *arg) {
  object *result = cons(NULL, NULL);
  object *ptr = result;
  while (arg != NULL) {
    cdr(ptr) = cons(car(arg), NULL);
    ptr = cdr(ptr); arg = cdr(arg);
  }
  return cdr(result);
}

/*
  upshift_bit - Destructively shifts a bignum up one bit; ie multiplies by 2.
*/
void upshift_bit (object *bignum) {
  uint32_t now = (uint32_t)checkinteger(car(bignum));
  car(bignum) = number(now << 1);
  while (cdr(bignum) != NULL) {
    uint32_t next = (uint32_t)checkinteger(car(cdr(bignum)));
    car(cdr(bignum)) = number((next << 1) | (now >> 31));
    now = next; bignum = cdr(bignum);
  }
  if (now >> 31 != 0) cdr(bignum) = cons(number(now >> 31), NULL);
}

/*
  downshift_bit - Destructively shifts a bignum down one bit; ie divides by 2.
*/
void downshift_bit (object *bignum) {
  uint32_t now = (uint32_t)checkinteger(car(bignum));
  while (cdr(bignum) != NULL) {
    uint32_t next = (uint32_t)checkinteger(car(cdr(bignum)));
    car(bignum) = number((now >> 1) | (next << 31));
    now = next; bignum = cdr(bignum);
  }
  car(bignum) = number(now >> 1);
}

/*
  bignum_from_int - Converts a 64-bit integer to a bignum and returns it.
*/
object *bignum_from_int (uint64_t n) {
  uint32_t high = n>>32;
  if (high == 0) return cons(number(n), NULL);
  return cons(number(n), cons(number(high), NULL));
}

/*
  bignum_add - Performs bignum1 + bignum2.
*/
object *bignum_add (object *bignum1, object *bignum2) {
  object *result = cons(NULL, NULL);
  object *ptr = result;
  int carry = 0;
  while (!(bignum1 == NULL && bignum2 == NULL)) {
    uint64_t tmp1 = 0, tmp2 = 0, tmp;
    if (bignum1 != NULL) {
      tmp1 = (uint64_t)(uint32_t)checkinteger(first(bignum1));
      bignum1 = cdr(bignum1);
    }
    if (bignum2 != NULL) {
      tmp2 = (uint64_t)(uint32_t)checkinteger(first(bignum2));
      bignum2 = cdr(bignum2);
    }
    tmp = tmp1 + tmp2 + carry;
    carry = (tmp > MAX_VAL);
    cdr(ptr) = cons(number(tmp & MAX_VAL), NULL);
    ptr = cdr(ptr);
  }
  if (carry != 0) {
    cdr(ptr) = cons(number(carry), NULL);
  }
  return cdr(result);
}

/*
  bignum_sub - Performs bignum1 = bignum1 - bignum2.
*/
 object *bignum_sub (object *bignum1, object *bignum2) {
  object *result = cons(NULL, NULL);
  object *ptr = result;
  int borrow = 0;
  while (!(bignum1 == NULL && bignum2 == NULL)) {
    uint64_t tmp1, tmp2, res;
    if (bignum1 != NULL) {
      tmp1 = (uint64_t)(uint32_t)checkinteger(first(bignum1)) + (MAX_VAL + 1);
      bignum1 = cdr(bignum1);
    } else tmp1 = (MAX_VAL + 1);
    if (bignum2 != NULL) {
      tmp2 = (uint64_t)(uint32_t)checkinteger(first(bignum2)) + borrow;
      bignum2 = cdr(bignum2);
    } else tmp2 = borrow;
    res = tmp1 - tmp2;
    borrow = (res <= MAX_VAL);
    cdr(ptr) = cons(number(res & MAX_VAL), NULL);
    ptr = cdr(ptr);
  }
  return cdr(result);
}

/*
  bignum_mul - Performs bignum1 * bignum2.
*/
object *bignum_mul (object *bignum1, object *bignum2, object *env) {
  object *result = int_to_bignum(0);
  object *arg2 = bignum2;
  int i = 0, j;
  while (bignum1 != NULL) {
    bignum2 = arg2; j = 0;
    while (bignum2 != NULL) {
      uint64_t n = (uint64_t)(uint32_t)checkinteger(first(bignum1)) *
        (uint64_t)(uint32_t)checkinteger(first(bignum2));
      object *tmp;
      if (n > MAX_VAL) tmp = cons(number(n), cons(number(n>>(uint64_t)32), NULL));
      else tmp = cons(number(n), NULL);
      for (int m = i + j; m > 0; m--) push(number(0), tmp); // upshift i+j words
      result = bignum_add(result, tmp);
      bignum2 = cdr(bignum2); j++;
      maybe_gc(result, env);
    }
    bignum1 = cdr(bignum1); i++;
  }
  return result;
}

/*
  bignum_div - Performs bignum1 / bignum2 and returns the list (quotient remainder).
  First we normalise the denominator, and then do bitwise subtraction.
  We need to do gcs in the main loops, while preserving the temporary lists on the GCStack.
*/
object *bignum_div (object *bignum1, object *bignum2, object *env) {
  object *current = int_to_bignum(1);
  object *denom = copylist(bignum2);
  while (bignum_cmp(denom, bignum1) != LARGER) {
    push(number(0), current); push(number(0), denom); // upshift current and denom 1 word
    push(current, GCStack);
    maybe_gc(denom, env);
    pop(GCStack);
  }

  object *result = int_to_bignum(0);
  object *remainder = copylist(bignum1);
  while (!bignum_zerop(current)) {
    if (bignum_cmp(remainder, denom) != SMALLER) {
      remainder = bignum_sub(remainder, denom);
      result = do_operator(result, current, op_ior);
    }
    downshift_bit(current); downshift_bit(denom);
    push(current, GCStack); push(remainder, GCStack); push(denom, GCStack);
    maybe_gc(result, env);
    pop(GCStack); pop(GCStack); pop(GCStack);
  }
  return cons(result, cons(remainder, NULL));
}

/*
  bignum_cmp - Compares two bignums and returns LARGER (b1>b2), EQUAL (b1=b2), or SMALLER (b1<b2).
  This uses a backwards comparison method that's more efficient because bignums have the LSB first.
*/
int bignum_cmp (object *bignum1, object *bignum2) {
  int state = EQUAL;
  uint32_t b1, b2;
  while (!(bignum1 == NULL && bignum2 == NULL)) {
    if (bignum1 != NULL) {
      b1 = checkinteger(car(bignum1));
      bignum1 = cdr(bignum1);
    } else b1 = 0;
    if (bignum2 != NULL) {
      b2 = checkinteger(car(bignum2));
      bignum2 = cdr(bignum2);
    } else b2 = 0;
    if (b1 > b2) state = LARGER; else if (b1 < b2) state = SMALLER;
  }
  return state;
}

uint32_t op_and (uint32_t a, uint32_t b) { return a & b; };
uint32_t op_ior (uint32_t a, uint32_t b) { return a | b; };
uint32_t op_xor (uint32_t a, uint32_t b) { return a ^ b; };

/*
  do_operator - Returns the result of performing a logical operation on two bignums.
*/
object *do_operator (object *bignum1, object *bignum2, uint32_t (*op)(uint32_t, uint32_t)) {
  object *result = cons(NULL, NULL);
  object *ptr = result;
  uint32_t tmp1 = 0, tmp2 = 0;
  while (!(bignum1 == NULL && bignum2 == NULL)) {
    if (bignum1 != NULL) {
      tmp1 = (uint32_t)checkinteger(first(bignum1));
      bignum1 = cdr(bignum1);
    }
    if (bignum2 != NULL) {
      tmp2 = (uint32_t)checkinteger(first(bignum2));
      bignum2 = cdr(bignum2);
    }
    cdr(ptr) = cons(number(op(tmp1, tmp2)), NULL);
    ptr = cdr(ptr);
  }
  return cdr(result);
}

// Lisp functions

/*
  ($bignum int)
  Converts an integer to a bignum and returns it.
*/
object *fn_Sbignum (object *args, object *env) {
  (void) env;
  return int_to_bignum(checkinteger(first(args)));
}

/*
  ($integer bignum)
  Converts a bignum to an integer and returns it.
*/
object *fn_Sinteger (object *args, object *env) {
  (void) env;
  object *bignum = checkbignum(first(args));
  bignum = bignum_normalise(bignum);
  uint32_t i = checkinteger(first(bignum));
  if (cdr(bignum) != NULL || i > 0x7FFFFFFF) error2(PSTR("bignum too large to convert to an integer"));
  return number(i);
}

/*
  ($bignum-string bignum [base])
  Converts a bignum to a string in base 10 (default) or 16 and returns it.
  Base 16 is trivial. For base 10 we get remainders mod 1000000000 and then print those.
*/
object *fn_Sbignumstring (object *args, object *env) {
  (void) env;
  object *bignum = copylist(checkbignum(first(args)));
  int b = 10; uint32_t p;
  args = cdr(args);
  if (args != NULL) b = checkinteger(car(args));
  object *list = NULL;
  if (b == 16) {
    p = 0x10000000;
    while (bignum != NULL) {
      push(car(bignum), list);
      bignum = cdr(bignum);
    }
  } else if (b == 10) {
    p = 100000000;
    object *base = cons(number(p*10), NULL);
    while(!bignum_zerop(bignum)) {
      push(bignum, GCStack); push(base, GCStack); push(list, GCStack);
      object *result = bignum_div(bignum, base, env);
      pop(GCStack); pop(GCStack); pop(GCStack);
      object *remainder = car(second(result));
      bignum = first(result);
      push(remainder, list);
    }
  } else error2(PSTR("only base 10 or 16 supported"));
  bool lead = false;
  object *obj = newstring();
  object *tail = obj;
  while (list != NULL) {
    uint32_t i = car(list)->integer;
    for (uint32_t d=p; d>0; d=d/b) {
      uint32_t j = i/d;
      if (j!=0 || lead || d==1) { 
        char ch = (j<10) ? j+'0' : j+'W';
        lead=true;
        buildstring(ch, &tail);
      }
      i = i - j*d;
    }
    list = cdr(list);
  }
  return obj;
}

/*
  ($string-bignum string [base])
  Converts a string in the specified base, 10 (default) or 16, to a bignum and returns it.
*/
object *fn_Sstringbignum (object *args, object *env) {
  (void) env;
  object *string = first(args);
  if (!stringp(string)) error(notastring, string);
  int b = 10;
  args = cdr(args);
  if (args != NULL) b = checkinteger(car(args));
  if (b != 10 && b != 16) error2(PSTR("only base 10 or 16 supported"));
  object *base = int_to_bignum(b);
  object *result = int_to_bignum(0);
  object *form = (object *)string->name;
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (!ch) break;
      int d = digitvalue(ch);
      if (d >= b) error(PSTR("illegal character in bignum"), character(ch));
      push(result, GCStack); push(base, GCStack);
      result = bignum_mul(result, base, env);
      pop(GCStack); pop(GCStack);
      result = bignum_add(result, cons(number(d), NULL));
    }
    form = car(form);
  }
  return result;
}

/*
  ($zerop bignum)
  Tests whether a bignum is zero, allowing for trailing zeros.
*/
object *fn_Szerop (object *args, object *env) {
  (void) env;
  return bignum_zerop(checkbignum(first(args))) ? tee : nil;
}

/*
  ($+ bignum1 bignum2)
  Adds two bignums and returns the sum as a new bignum.
*/
object *fn_Sadd (object *args, object *env) {
  (void) env;
  return bignum_add(checkbignum(first(args)), checkbignum(second(args)));
}

/*
  ($- bignum1 bignum2)
  Subtracts two bignums and returns the difference as a new bignum.
*/
object *fn_Ssub (object *args, object *env) {
  (void) env;
  return bignum_sub(checkbignum(first(args)), checkbignum(second(args)));
}

/*
  ($* bignum1 bignum2)
  Multiplies two bignums and returns the product as a new bignum.
*/
object *fn_Smul (object *args, object *env) {
  return bignum_mul(checkbignum(first(args)), checkbignum(second(args)), env);
}

/*
  ($/ bignum1 bignum2)
  Divides two bignums and returns the quotient as a new bignum.
*/
object *fn_Sdiv (object *args, object *env) {
  return first(bignum_div(checkbignum(first(args)), checkbignum(second(args)), env));
}

/*
  ($mod bignum1 bignum2)
  Divides two bignums and returns the remainder as a new bignum.
*/
object *fn_Smod (object *args, object *env) {
  return bignum_normalise(second(bignum_div(checkbignum(first(args)), checkbignum(second(args)), env)));
}

// Comparisons
/*
  ($= bignum1 bignum2)
  Returns t if the two bignums are equal.
*/
object *fn_Sequal (object *args, object *env) {
  (void) env;
  return (bignum_cmp(checkbignum(first(args)), checkbignum(second(args))) == EQUAL) ? tee : nil;
}

/*
  ($< bignum1 bignum2)
  Returns t if bignum1 is less than bignum2.
*/
object *fn_Sless (object *args, object *env) {
  (void) env;
  return (bignum_cmp(checkbignum(first(args)), checkbignum(second(args))) == SMALLER) ? tee : nil;
}

/*
  ($> bignum1 bignum2)
  Returns t if bignum1 is greater than bignum2.
*/
object *fn_Sgreater (object *args, object *env) {
  (void) env;
  return (bignum_cmp(checkbignum(first(args)), checkbignum(second(args))) == LARGER) ? tee : nil;
}

// Bitwise logical operations

/*
  ($logand bignum1 bignum2)
  Returns the logical AND of two bignums.
*/
object *fn_Slogand (object *args, object *env) {
  (void) env;
  return bignum_normalise(do_operator(checkbignum(first(args)), checkbignum(second(args)), op_and));
}

/*
  ($logior bignum1 bignum2)
  Returns the logical inclusive OR of two bignums.
*/
object *fn_Slogior (object *args, object *env) {
  (void) env;
  return bignum_normalise(do_operator(checkbignum(first(args)), checkbignum(second(args)), op_ior));
}

/*
  ($logxor bignum1 bignum2)
  Returns the logical exclusive OR of two bignums.
*/
object *fn_Slogxor (object *args, object *env) {
  (void) env;
  return bignum_normalise(do_operator(checkbignum(first(args)), checkbignum(second(args)), op_xor));
}

/*
  ($ash bignum shift)
  Returns bignum shifted by shift bits; positive means left.
*/
object *fn_Sash (object *args, object *env) {
  (void) env;
  object *bignum = copylist(checkbignum(first(args)));
  int shift = checkinteger(second(args));
  for (int i = 0; i < shift; i++) upshift_bit(bignum);
  for (int i = 0; i < -shift; i++) downshift_bit(bignum);
  return bignum_normalise(bignum);
}

// Symbol names
const char stringSbignum[] PROGMEM = "$bignum";
const char stringSinteger[] PROGMEM = "$integer";
const char stringSbignumstring[] PROGMEM = "$bignum-string";
const char stringSstringbignum[] PROGMEM = "$string-bignum";
const char stringSzerop[] PROGMEM = "$zerop";
const char stringSadd[] PROGMEM = "$+";
const char stringSsub[] PROGMEM = "$-";
const char stringSmul[] PROGMEM = "$*";
const char stringSdiv[] PROGMEM = "$/";
const char stringSmod[] PROGMEM = "$mod";
const char stringSequal[] PROGMEM = "$=";
const char stringSless[] PROGMEM = "$<";
const char stringSgreater[] PROGMEM = "$>";
const char stringSlogand[] PROGMEM = "$logand";
const char stringSlogior[] PROGMEM = "$logior";
const char stringSlogxor[] PROGMEM = "$logxor";
const char stringSash[] PROGMEM = "$ash";

// Documentation strings
const char docSbignum[] PROGMEM = "($bignum int)\n"
"Converts an integer to a bignum and returns it.";
const char docSinteger[] PROGMEM = "($integer bignum)\n"
"Converts a bignum to an integer and returns it.";
const char docSbignumstring[] PROGMEM = "($bignum-string bignum [base])\n"
"Converts a bignum to a string in base 10 (default) or 16 and returns it.";
const char docSstringbignum[] PROGMEM = "($string-bignum bignum [base])\n"
"Converts a bignum to a string in the specified base (default 10) and returns it.";
const char docSzerop[] PROGMEM = "($zerop bignum)\n"
"Tests whether a bignum is zero, allowing for trailing zeros.";
const char docSadd[] PROGMEM = "($+ bignum1 bignum2)\n"
"Adds two bignums and returns the sum as a new bignum.";
const char docSsub[] PROGMEM = "($- bignum1 bignum2)\n"
"Subtracts two bignums and returns the difference as a new bignum.";
const char docSmul[] PROGMEM = "($* bignum1 bignum2)\n"
"Multiplies two bignums and returns the product as a new bignum.";
const char docSdiv[] PROGMEM = "($/ bignum1 bignum2)\n"
"Divides two bignums and returns the quotient as a new bignum.";
const char docSmod[] PROGMEM = "($mod bignum1 bignum2)\n"
"Divides two bignums and returns the remainder as a new bignum.";
const char docSequal[] PROGMEM = "($= bignum1 bignum2)\n"
"Returns t if the two bignums are equal.";
const char docSless[] PROGMEM = "($< bignum1 bignum2)\n"
"Returns t if bignum1 is less than bignum2.";
const char docSgreater[] PROGMEM = "($> bignum1 bignum2)\n"
"Returns t if bignum1 is greater than bignum2.";
const char docSlogand[] PROGMEM = "($logand bignum bignum)\n"
"Returns the logical AND of two bignums.";
const char docSlogior[] PROGMEM = "($logior bignum bignum)\n"
"Returns the logical inclusive OR of two bignums.";
const char docSlogxor[] PROGMEM = "($logxor bignum bignum)\n"
"Returns the logical exclusive OR of two bignums.";
const char docSash[] PROGMEM = "($ash bignum shift)\n"
"Returns bignum shifted by shift bits; positive means left.";

// Symbol lookup table
const tbl_entry_t lookup_table2[] PROGMEM = {
  { stringSbignum, fn_Sbignum, 0211, docSbignum },
  { stringSinteger, fn_Sinteger, 0211, docSinteger },
  { stringSbignumstring, fn_Sbignumstring, 0212, docSbignumstring },
  { stringSstringbignum, fn_Sstringbignum, 0212, docSstringbignum },
  { stringSzerop, fn_Szerop, 0211, docSzerop },
  { stringSadd, fn_Sadd, 0222, docSadd },
  { stringSsub, fn_Ssub, 0222, docSsub },
  { stringSmul, fn_Smul, 0222, docSmul },
  { stringSdiv, fn_Sdiv, 0222, docSdiv },
  { stringSmod, fn_Smod, 0222, docSmod },
  { stringSequal, fn_Sequal, 0222, docSequal },
  { stringSless, fn_Sless, 0222, docSless },
  { stringSgreater, fn_Sgreater, 0222, docSgreater },
  { stringSlogand, fn_Slogand, 0222, docSlogand },
  { stringSlogior, fn_Slogior, 0222, docSlogior },
  { stringSlogxor, fn_Slogxor, 0222, docSlogxor },
  { stringSash, fn_Sash, 0222, docSash },
};

// Table cross-reference functions - do not edit below this line

tbl_entry_t *tables[] = {lookup_table, lookup_table2};
const unsigned int tablesizes[] = { arraysize(lookup_table), arraysize(lookup_table2) };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}
