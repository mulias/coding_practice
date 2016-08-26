/* Skiena 8-1. 
 * Adding a swap option to the edit distence algorithm.
 * 
 * To make this change I:
 * added a new enumerated value for swap
 * in string_compare, check if swapping the current char and the next char in
 *   the input string will make the current and next char match the output
 * when saving an operation to the table, check if it comes after a swap,
 *   and if it does make this second opeation cost 0
 *
 * I also refactored Skiena's code a bit, because I thought the way he did
 * characters that mach/do not was confusing.
 *
 * The result of running the two tests:
 * edit distance from " thou shalt not" to " you should not" is 5: DCMMMMMICMCMMMM
 * edit distance from " setve" to " steve" is 1: MSSMM
 *
 * This code is based on code found on pages 281 to 288 of Steven Skiena's 
 * 'The Algorithm Design Manual' 
 */

#include <stdio.h>
#include <string.h>

// possible chracter operations on the input string
#define MATCH  0  // chars are the same
#define SWAP   1  // two chars in a row are reversed
#define CHANGE 2  // char is changed to a different char
#define INSERT 3  // add a new char after this one
#define DELETE 4  // delete the current char

// infinity is a big number
#define INF 9999

// input strings can not be longer this
#define MAXLEN 20

typedef struct {
  int cost;
  int parent;
} cell;

cell m[MAXLEN+1][MAXLEN+1];

// set the cost of changing the empty string to the output string
void row_init(int i)
{
  m[0][i].cost = i;
  if(i>0)
    m[0][i].parent = INSERT;
  else
    m[0][i].parent = -1;
}

// set the cost of changing the full input string to the empty string
void column_init(int i)
{
  m[i][0].cost = i;
  if(i>0)
    m[i][0].parent = DELETE;
  else
    m[i][0].parent = -1;
}

// if c and d are the same, we can skip them for 0. Otherwise match is not a
// valid operation, so it's worth infinity.
int match(char c, char d)
{
  if (c == d)
    return(0);
  else
    return(INF);
}

// if s = "ab" and t = "ba", we can switch the two chars for a cost of 1.
// Otherwise swap is not a valid option, so it's worth infinity.
int swap(char* s, char* t, int i)
{
  if (i+1 < strlen(s) && i+1 < strlen(t) && 
      s[i] == t[i+1] && s[i+1] == t[i])
    return(1);
  else
    return(INF);
}

// set out indecies i and j to the table cell that contains the last operation
// which converts the input string to the output string.
void goal_cell(char* s, char* t, int* i, int* j)
{
  *i = strlen(s) - 1;
  *j = strlen(t) - 1;
}

// return the edit distance for changing s into t by means of matching, swapping,
// changing, inserting, or deleting chars.
int string_compare(char* s, char* t)
{
  int i,j,k;
  int opt[5];

  for (i = 0; i < MAXLEN; i++)
  {
    row_init(i);
    column_init(i);
  }

  for (i = 1; i < strlen(s); i++)
  {
    for (j = 1; j < strlen(t); j++)
    {
      opt[MATCH]  = m[i-1][j-1].cost + match(s[i],t[j]);
      opt[SWAP]   = m[i-1][j-1].cost + swap(s, t, i);
      opt[CHANGE] = m[i-1][j-1].cost + 1;
      opt[INSERT] = m[i][j-1].cost + 1;
      opt[DELETE] = m[i-1][j].cost + 1;

      m[i][j].cost = opt[MATCH];
      m[i][j].parent = MATCH;
      for (k = SWAP; k <= DELETE; k++)
      {
        if (opt[k] < m[i][j].cost)
        {
          // if the last in-place action was a swap, and this action is a
          // change, then we are actually continuing the swap. Swap changes
          // two characters for a cost of one, so we change the first char for
          // 1, and change the second char for 0.
          if (k == CHANGE && m[i-1][j-1].parent == SWAP)
            m[i][j].cost = m[i-1][j-1].cost;
          // otherwise it's the least cost action, as identified
          else
            m[i][j].cost = opt[k];
          
          m[i][j].parent = k;
        }
      }
    }
  }

  goal_cell(s,t,&i,&j);
  return(m[i][j].cost);
}

//print the shortest sequence of modifications that goes from s to t.
//reads from the global table, so only works after string_compare is called.
void reconstruct_path(char* s, char* t, int i, int j)
{
  switch(m[i][j].parent)
  {
    case -1     : return;
    case MATCH  : reconstruct_path(s,t,i-1,j-1);
                  printf("M");
                  return;
    case INSERT : reconstruct_path(s,t,i,j-1);
                  printf("I");
                  return;
    case DELETE : reconstruct_path(s,t,i-1,j);
                  printf("D");
                  return;
    case CHANGE : reconstruct_path(s,t,i-1,j-1);
                  if (m[i-1][j-1].parent == SWAP)
                    printf("S");
                  else
                    printf("C");
                  return;
    case SWAP   : reconstruct_path(s,t,i-1,j-1);
                  printf("S");
                  return;
  }
}

// print the start and target string, the edit distence, and the edit sequence.
// the space for the first character has to be manually added.
void test(char* str1, char* str2)
{
  int res = string_compare(str1, str2);
  printf("edit distance from \"%s\" to \"%s\" is %d: ", str1, str2, res);
  reconstruct_path(str1, str2, strlen(str1) - 1, strlen(str2) - 1);
  printf("\n");
}

int main()
{
  // only one test works at a time because I didn't bother clearing the matrix
  //test(" thou shalt not", " you should not");
  test(" setve", " steve");
  return(0);
}
