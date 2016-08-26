#####
# In bubble sort we start at the beginning of the array and compare each
# pair of adjacent elements. If the elements are in the wrong order, swap.
# Keep looping through the array until an iteration traverses the entire
# array without swapping any elements. Bubble sort has O(n^2) runtime.
#
# in_arr is the input array. in_arr is not changed by bubble_sort.
#
# ordered is a proc that takes two elements and returns true if they are in the
# intended order.
#
# bubble_sort returns a new array sorted by the rule defined in the ordered proc
def bubble_sort (in_arr, ordered)
  arr = Array.new(in_arr)
  len = arr.length
  # compare each element with the element before it
  # every time we swap two elements, start again from the beginning
  # if we get to the end without swapping, done
  pos = 1
  while pos < len
    # if two elements are not ordered
    # swap and reset to start
    if !ordered.call(arr[pos-1], arr[pos])
      swap_index(arr, pos-1, pos)
      pos = 1
    # if elements are ordered, go to next pair
    else
      pos += 1
    end
  end
  return arr
end

#####
# In selection sort we divide the array into a sorted section and a unsorted 
# section. We linearly searth the unsorted section to find the smallest unsorted
# value, and add it to the sorted section. When the size of the sorted section
# is the size of the array we are done. Selection sort has O(n^2) runtime.
#
# in_arr is the input array. in_arr is not changed by selection_sort.
#
# ordered is a proc that takes two elements and returns true if they are in the
# intended order.
#
# selection_sort returns a new array sorted by the rule defined by ordered proc
def selection_sort (in_arr, ordered)
  arr = Array.new(in_arr)
  len = arr.length
  # sorted is the size of the sorted array
  # and also the index of first unsorted element
  sorted = 0
  while sorted < len
    next_to_sort = sorted + first_ordered_index(arr[sorted..-1], ordered) 
    swap_index(arr, sorted, next_to_sort)
    sorted += 1
  end
  return arr
end

def first_ordered_index (arr, ordered)
  # first is  the index of element that should go first if ordered
  first = 0
  # Check first against each element. If elem should go before first
  # then elem is the new first
  arr.each_with_index do |elem, i|
    if !ordered.call(arr[first], elem)
      first = i
    end
  end
  return first
end

#####
# Merge sort splits the array in half, then recursivly splits each half in half
# until each subarray is only one or two elements. We merge small subarrays
# together into larger sorted subarrays, then merge those together until the
# full array is constructed in order. Merge sort has O(n*log(n)) runtime.
#
# in_arr is the input array. It is not changed by merge_sort.
#
# ordered is a proc that takes two elements and returns true if they are in the
# intended order.
#
# merge_sort returns a new array sorted by the rule defined by the ordered proc
def merge_sort (in_arr, ordered)
  arr = Array.new(in_arr)
  helper = Array.new(arr.length)
  merge_kernel(arr, helper, 0, arr.length-1, ordered)
  return arr
end

# The kernel recursivly implements merge sort
# arr is the array to sort
# helper is a helper array to copy tmp work to
# low is the lowest index in the subarray, inclusive
# high is the highest index in the subarray, inclusive
# ordered is the ordering proc
def merge_kernel(arr, helper, low, high, ordered)
  if low < high
    mid = (low + high)/2
    merge_kernel(arr, helper, low, mid, ordered)
    merge_kernel(arr, helper, mid+1, high, ordered)
    merge(arr, helper, low, mid, high, ordered)
  end
end

# merge two subarrays together into a larger sorted subarray
# arr is the array to sort
# helper is a helper array to copy tmp work to
# low is the lowest index of the left subarray, inclusive
# mid is the highest index of the left subarray, inclusive
# high is the highest index of the right subarray, inclusive
# ordered is the ordering proc
def merge (arr, helper, low, mid, high, ordered)
  # copy subarray to helper
  for i in low..high
    helper[i] = arr[i]
  end
  # track position of both subarrays in helper
  # track current position of elements added to arr
  h_left = low
  h_right = mid + 1
  current = low 

  # compare and copy until we finish one subarray
  while (h_left <= mid) && (h_right <= high)
    # if h_left goes before h_right
    # copy h_left to next position in arr
    if ordered.call(helper[h_left], helper[h_right])
      arr[current] = helper[h_left]
      h_left += 1
    # otherwise copy helper_right
    else
      arr[current] = helper[h_right]
      h_right += 1
    end
    current += 1
  end

  # if left subarray is not all copied back to arr, finish
  leftover = mid - h_left
  for i in 0..leftover
    arr[current + i] = helper[h_left + i]
  end

  # if right subarray is not all copied back, do nothing
  # Unsorted right subarray elements are on the far right, where they would not
  # have been copied over by smaller elements,  so they are still in the same 
  # place in arr.
end

#####
# Quick sort selects a random element to be the 'pivot' and swaps elements
# around the pivot so that all elements to the left of the pivot go before the
# pivot, and all elements to the right go after the pivot. We then call quick
# sort on both the right and left subarrays recursivly, until they are ordered.
# Quick sort generally has O(n*log(n)) runtime, but if the pivot is in a bad
# place it can get up to O(n^2) runtime. The center of the array is not 
# necessarily the best place for the pivot, so we minimize the issue some by
# picking a random location.
#
# in_arr is the input array. It is not change by quick_sort.
#
# ordered is a proc that takes two elements and returns true if they are in the
# intended order.
#
# quick_sort returns a new array sorted by the rule defined by the ordered proc
def quick_sort (in_arr, ordered)
  arr = Array.new(in_arr)
  quick_kernel(arr, 0, arr.length-1, ordered)
  return arr
end

# The kernel recursivly implements merge sort
#
# arr is the full array
# low is the lowest index in the subarray, inclusive
# high is the highest index in the subarray, inclusive
# ordered is the ordering proc
def quick_kernel (arr, low, high, ordered)
  # partition so everything in right partiton comes before the pivot value
  # everything in left partition comes after the pivot value
  # pivot_index is the location of the pivot, pivot is not in either subarray 
  pivot_index = partition(arr, low, high, ordered)
  # sort both partitions, if the partition has at least one element in it
  quick_kernel(arr, low, pivot_index-1, ordered) if low < pivot_index-1
  quick_kernel(arr, pivot_index+1, high, ordered) if pivot_index+1 < high 
end

# partition array from index low to high into a subarray of elements less than
# the pivot, and a subarray of elements greater than the pivot.
#
# arr is the full array
# low is lowest index in the subarray, inclusive
# high is the highest index in the subarray, inclusive
# ordered is the ordering proc
#
# partition returns the index of the pivot, which is between the two subarrays
def partition (arr, low, high, ordered)
  # the pivot is a value in the subarray from low to high
  pivot_index = low + rand(high - low + 1)
  pivot_val = arr[pivot_index]
  # move the pivot value to the end so we can keep track of it
  swap_index(arr, pivot_index, high)
  # small_part is the partition of small elements. It starts as size 0 at the
  # start of the subarray. We scan each element, and if elem < pivot, move elem
  # to the small partition, and more small_part up. 
  small_part = low
  for i in low..(high-1)
    # ordered returns true when arr[i] <= pivot_val
    # so it also returns true when !(pivot_val <= arr[i]) == pivot_val > arr[i]
    if !ordered.call(pivot_val, arr[i])
      swap_index(arr, i, small_part)
      small_part += 1
    end
  end
  swap_index(arr, small_part, high)
  return small_part
end

#####
# General Helpers
def swap_index (arr, x, y)
  arr[x], arr[y] = arr[y], arr[x]
end

#####
# Quick tests 

order_num_small_first = Proc.new { |i,j| i <= j }
order_num_big_first = Proc.new { |i,j| i >= j }
order_strlen_small_first = Proc.new { |a,b| a.to_s.length <= b.to_s.length }

arr = [12, 4, 2.2, 6, 7, 11, 5, 4, 4, 4, 5, 2, 5, 99, 2, 8]
arr2 = [1]
arr3 = [12, 4, 2.2, 5, 4, 4, 4, 5, 2, 5, 99, 2, 8]
arr4 = ["a", "bf", 33, :qqqqq, 'aaa  aaa', "\n", :e3ddddddddd]

puts "bubble sort:"
p bubble_sort(arr, order_num_small_first)
p bubble_sort(arr, order_num_big_first)
p bubble_sort(arr2, order_num_big_first)
p bubble_sort(arr4, order_strlen_small_first)
puts "selection sort:"
p selection_sort(arr, order_num_small_first)
p selection_sort(arr, order_num_big_first)
p selection_sort(arr2, order_num_big_first)
p selection_sort(arr4, order_strlen_small_first)
puts "merge sort:"
p merge_sort(arr, order_num_small_first)
p merge_sort(arr, order_num_big_first)
p merge_sort(arr2, order_num_big_first)
p merge_sort(arr3, order_num_small_first)
p merge_sort(arr4, order_strlen_small_first)
puts "quick sort:"
p quick_sort(arr, order_num_small_first)
p quick_sort(arr, order_num_big_first)
p quick_sort(arr2, order_num_big_first)
p quick_sort(arr3, order_num_small_first)
p quick_sort(arr4, order_strlen_small_first)
