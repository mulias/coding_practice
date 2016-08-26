
/* Elias Mulhall
 * 10/12/2015
 *
 * CSC301: Algorithms, Exam 1
 *
 * Merge Sort with n/2 overhead space.
 * An implementation of merge sort that uses a helper array of size n/2, instead
 * of the standard helper array of size n. Provided as suplemental material
 * for question 5 of the exam.
 */

#include <assert.h>

/* HEADERS */
void merge_sort(int* arr, int len);
void merge_kernel(int* arr, int* helper, int low, int high);
void merge(int* arr, int* helper, int low, int mid, int high);
void test_merge_sort();
int comp_int_arr(int* arr1, int* arr2, int len);

/* main
 * Run basic tests for merge sort.
 */
int main ()
{
  test_merge_sort();
  return 0;
}

/* merge_sort 
 * Merge sort splits the array in half, then recursivly splits each half in half
 * until each subarray is only one or two elements. We merge small subarrays
 * together into larger sorted subarrays, then merge those together until the
 * full array is constructed in order. Merge sort has O(n*log(n)) runtime.
 *
 * arr is the input array, which will be modified.
 * len is the length of arr.
 */
void merge_sort(int* arr, int len)
{
  int helper_len = len/2 + (len % 2 != 0);
  int helper[helper_len];
  merge_kernel(arr, helper, 0, len);
}

/* merge_kernel
 * Recursively implement merge sort. Split the array into two subarrays, call
 * merge_kernel on both subarrays, then merge the two subarrays together. The
 * recursive case ends when the input array has a length of 1 or 2 elements.
 *
 * arr is the array to sort
 * helper is a intermediary array
 * low is the lowest index in the subarray, inclusive
 * high is the highest index in the subarray, exclusive
 */
void merge_kernel(int* arr, int* helper, int low, int high)
{
  if(low+1 < high)
  {
    int mid = (low + high)/2 + ((low + high) % 2 != 0);
    merge_kernel(arr, helper, low, mid);
    merge_kernel(arr, helper, mid, high);
    merge(arr, helper, low, mid, high);
  }
}

/* merge
 * Take two consecutive subarrays, one from index low to mid-1 and one from 
 * index mid to high-1, and perge them back together into one sorted subarray
 * from index low to high-1.
 *
 * arr is the array to sort
 * helper is an intermediary array
 * low is the lowest inex of subarray 1, inclusive
 * mid is the highest inex of sub1, exclusive, and lowest of sub2, inclusive
 * high is the highest index of sub2, exclusive
 */
void merge(int* arr, int* helper, int low, int mid, int high)
{
  int i;
  for(i = low; i < mid; i++)
  {
    helper[i] = arr[i];
  }

  int h_left = low;
  int a_right = mid;
  int current = low;

  while(h_left < mid && a_right < high)
  {
    if(helper[h_left] < arr[a_right])
    {
      arr[current] = helper[h_left];
      h_left++;
    }
    else
    {
      arr[current] = arr[a_right];
      a_right++;
    }
    current++;
  }

  int leftover = mid - h_left;
  for(i = 0; i < leftover; i++)
  {
    arr[current + i] = helper[h_left + i];
  }
}

int comp_int_arr(int* arr1, int* arr2, int len)
{
  int i;
  for(i = 0; i < len; i++)
  {
    if(arr1[i] != arr2[i])
      return 0;
  }
  return 1;
}

/* test_merge_sort
 * A few simple tests
 */
void test_merge_sort()
{
  int u1[11] = {7, 4, 3, 2, 1, 8, 3, 32, 12, 4, 6};
  int s1[11] = {1, 2, 3, 3, 4, 4, 6, 7, 8, 12, 32};
  merge_sort(u1, 11);
  assert(comp_int_arr(u1, s1, 11));

  int u2[12] = {1, 4, 1, 2, 1, 8, 1, 32, 1, 4, 1, 666};
  int s2[12] = {1, 1, 1, 1, 1, 1, 2, 4, 4, 8, 32, 666};
  merge_sort(u2, 12);
  assert(comp_int_arr(u2, s2, 12));

  int u3[3] = {8, 6, 1};
  int s3[3] = {1, 6, 8};
  merge_sort(u3, 3);
  assert(comp_int_arr(u3, s3, 3));
}
