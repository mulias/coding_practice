def assert_equal expected, result
  if !(expected == result)
    raise "test failed: #{expected} != #{result}"
  end
end
    

# 1.1 Implement an algorithm to determine if a string has all unique characters.

def str_chars_unique? str
  checker = {}
  str.codepoints.each do |c|
    if checker[c] == nil
      checker[c] = true
    else
      return false
    end
  end
  return true
end

assert_equal(true, str_chars_unique?("abcfgq"))
assert_equal(false, str_chars_unique?("abvfgthyua"))
assert_equal(false, str_chars_unique?("fffffqqqqq"))


# 1.2 Implement reverse.

def reverse str
  for pos in 0..((str.length/2)-1) do
    swap_pos = str.length - pos - 1
    tmp = str[pos]
    str[pos] = str[swap_pos]
    str[swap_pos] = tmp
  end
  return str
end

assert_equal("tset", reverse("test"))
assert_equal("asdfghjkl", reverse("lkjhgfdsa"))
assert_equal("", reverse(""))

# 1.3 Given two strings, write a method to decide if one is a permutation of the
# other.

def str_permutation? strA, strB
  return false if strA.length != strB.length
  char_counts = Hash.new(0)
  strA.codepoints.each { |c| char_counts[c] = char_counts[c] + 1 }
  strB.codepoints.each { |c| char_counts[c] = char_counts[c] - 1 }
  char_counts.all? { |_,count| count == 0 }
end

assert_equal(true, str_permutation?("a", "a"))
assert_equal(false, str_permutation?("aa", "a"))
assert_equal(false, str_permutation?("ab", "ac"))
assert_equal(true, str_permutation?("0192384675", "0987654321"))
