/* Elias Mulhall
 * 9/15/2015
 *
 * CSC301: Algorithms, homework 2
 *
 * Quickselect. Licensed GPL.
 * The purpose of this exercise is to examin the running time of quickselect, so
 * to that end the function print_json_times outputs a json array of runtime
 * data, listing a number of different sized arrays and the cpu clocktime of
 * quickselect for 50 trials with each array size. I converted the json to
 * csv, opened it in libreoffice, averaged the 50 trials for each array size,
 * and graphed clock ticks vs array size for quickselect.
 *
 * Work Cited:
 * For implementing quickselect:
 * https://en.wikipedia.org/wiki/Quickselect
 * Skiena pg 124
 * For remembering C:
 * man pages for clock(3), assert(3), rand(3)
 * http://www.tutorialspoint.com/c_standard_library/c_function_clock.htm
 * json->csv conversion:
 * http://www.convertcsv.com/json-to-csv.htm
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>

/* HEADERS */

/* QUICKSELECT */
// Return kth smallest value in array (0-indexed).
int quickselect(int* arr, int length, int k);
// Recursively sort array until kth smallest is found.
void quickselect_kernel(int* arr, int k, int low, int high);
// Split array into subarrays < and >= pivot, return index of pivot.
int partition(int* arr, int low, int high);

/* HELPERS */
// Swap the values stored at arr[x] and arr[y].
void swap_index(int* arr, int x, int y);
// Mix up the values in an array. Hopefully the output is random enough.
void shuffle_array(int* arr, int length, int iters);

/* TESTING */
// Test quicksort, assert failure.
void test_quickselect();
// Output runtime data as json.
void print_json_times();



/* main
 * run all the tests, if the tests pass print runtime data
 */
int main ()
{
  test_quickselect();
  print_json_times();
  return 0;
}


/* QUICKSELECT */

/* quickselect
 * Quickselect uses the quicksort sorting method to sort an array, but stops
 * sorting when index k is properly sorted (meaning the k-1 things before the
 * kth thing are all smaller than k). Quickselect returns the kth smallest 
 * value, and as a side effect the array is left partially sorted.
 * Quicksort selects a random element to be the 'pivot' and swaps elements
 * around the pivot so that all elements to the left of the pivot are smaller
 * than the pivot, and all elements to the right are greater than or equal to
 * the pivot. We then call quicksort on both the left and right subarrays
 * recursively, until they are ordered.
 * To adapt quicksort for quickselect, we only recurse on the partition that
 * we know includes the kth smallest value, instead of both partitions.
 *
 * arr is the full array
 * length is the length of arr
 * k is the zero-indexed position of the target element
 *   (so 0th smallest up to (length-1)th smallest)
 */
int quickselect(int* arr, int length, int k)
{
  // call the recursive kernel function on the full array, then return the final
  // value when the array is partitioned around index k. Note that the lower
  // bound is 0 and upper bound is length-1, because both bounds are inclusive.
  quickselect_kernel(arr, k, 0, length-1);
  return arr[k];
}

/* quickselect_kernel
 * Recursively implement quickselect. Does not return a result, but modifies
 * the input array so that the kth smallest value is at index k.
 *
 * arr is the full array
 * k is the index to sort up to
 * low is lowest index in the subarray, inclusive
 * high is the highest index in the subarray, inclusive
 */
void quickselect_kernel(int* arr, int k, int low, int high)
{
  // partition returns the pivot index, and we choose which side of the pivot 
  // to sort depending on if the index is before or after index k.
  int pivot_index = partition(arr, low, high);
  // sort the lower partition, if it needs sorting
  if((k < pivot_index) && (low < pivot_index-1))
  {
    quickselect_kernel(arr, k, low, pivot_index-1);
  }
  // sort the upper partition, if it needs sorting
  else if(pivot_index+1 < high)
  {
    quickselect_kernel(arr, k, pivot_index+1, high);
  }
}

/* partition
 * Divide the array into a subarray of elements less than the pivot, and a 
 * subarray of elements greater than or equal to the pivot.
 *
 * arr is the full array
 * k is the index to sort up to
 * low is lowest index in the subarray, inclusive
 * high is the highest index in the subarray, inclusive
 */
int partition(int* arr, int low, int high)
{
  // just keep seeding random
  // (but really, how often should I do this?)
  srand (time(NULL));
  // pivot is a value in the subarray
  int pivot_index = low + (rand() % (high - low + 1));
  int pivot_val = arr[pivot_index];
  // move the pivot to the end so we can keep track of it
  swap_index(arr, pivot_index, high);
  // small_part is the partition of small elements. It starts as size 0 at the
  // start of the subarray. We scan each elem, and if elem < pivot, move elem
  // to the small partition, and move small_part up. 
  int small_part = low;
  // we stored the pivot at arr[high], so we only go up through (high-1)
  // it would be equivalent to say 'i < high', but this way is an explicit
  // reminder that something is at the last index.
  int i;
  for(i = low; i <= (high-1); i++)
  {
    if(arr[i] < pivot_val)
    {
      swap_index(arr, i, small_part);
      small_part++;
    }
  }
  // done partitioning, move pivot back to the middle
  swap_index(arr, small_part, high);

  return small_part;
}

/* HELPERS */

/* swap_index
 * Helper function, swaps the values stored at arr[x] and arr[y].
 *
 * arr is the array
 * x is the index of the first element
 * y is the index of the second element
 */
void swap_index(int* arr, int x, int y)
{
  int tmp = arr[x];
  arr[x] = arr[y];
  arr[y] = tmp;
}

/* shuffle_array
 * Mix up the values in an array. Hopefully the output is random enough.
 *
 * arr is the array
 * length is the length of arr
 * iters is the number of times to loop through the array and shuffle each elem
 */
void shuffle_array(int* arr, int length, int iters)
{
  int iter, elem; // counters

  // seed random so that we get pseudo-random results
  srand (time(NULL));

  // start at index 0, swap the element at each index with another random 
  // element, go through array iters times.
  for(iter = 0; iter < iters; iter++)
  {
    for(elem = 0; elem < length; elem++)
    {
      int swap_to = rand() % length;
      swap_index(arr, elem, swap_to);
    }
  }
}

/* TESTING */

/* test_quickselect
 * Test quickselect on every index of every array from size 1 to 100. That's
 * over 5000 random tests of many positions in many different sized arrays. For
 * testing large arrays note that the json exporting function also tests results
 * as it goes, so that speradically tests values in arrays ranging in size from 
 * 1000 to 100000.
 */
void test_quickselect()
{
  int arr_size, test, i; // iterator

  for(arr_size = 1; arr_size <= 100; arr_size++)
  {
    int arr[arr_size]; // array to sort
    // run quickselect on each element i, test result
    // quickselect(i) should find that the ith value is i
    for(test = 0; test < arr_size; test++)
    {
      // set the array
      for(i = 0; i < arr_size; i++)
      {
        arr[i] = i;
      }
      // mix it up!
      // do it 10 times just to be sure.
      shuffle_array(arr, arr_size, 10);

      // select!
      quickselect(arr, arr_size, test);
      assert (arr[test] == test);
    }
  }
}

/* print_json_time
 * Runs through arrays from size 1000 to 100000 and executes 50 quickselect
 * trials per array. The resulting cpu clock time is outputed as json to stdout.
 * The resulting json can be converted to a cvs file, which can be viewed in
 * libreoffice.
 */
void print_json_times()
{
  clock_t start, end, total; // track cpu cycles
  int size, i, j;            // iterators

  // start json array
  printf("[\n");

  for(size = 1000; size <= 100000; size+=3000)
  {
    int arr[size];

    //start json row
    printf("{\n");
    printf("size:%d, ", size);
    // do 50 trials
    for(i = 0; i < 50; i++)
    {
      // the number to find in this trial
      int target = i*(size/50);

      // set up the array
      for(j = 0; j < size; j++)
      {
        arr[j] = j;
      }
      // mix it up!
      shuffle_array(arr, size, 10);

      // get clock value before and after selection
      start = clock();
      quickselect(arr, size, target);
      end = clock();
      total = end - start;

      // test just to make sure
      assert (arr[target] == target);

      printf("trial%d:%d, ", i, total);
    }
    // end json row
    printf("\n},\n");
  }
  // end json array
  printf("]\n");
}

