def assert_equal expected, result
  if !(expected == result)
    raise "test failed: #{expected} != #{result}"
  end
end

class Tree

  attr_reader :key, :val

  def initialize compare
    @key   = nil
    @val   = nil
    @left   = nil
    @right  = nil
    @compare = compare
  end

  def empty?
    @key == nil
  end

  def leaf?
    !self.empty? && @left.empty? && @right.empty?
  end

  def branch?
    !self.empty? && !self.leaf?
  end

  def add new_key, new_val
    # empty, make leaf
    if self.empty?
      @key = new_key
      @val = new_val
      @left  = Tree.new(@compare)
      @right = Tree.new(@compare)
    # add to right or left of leaf/branch
    else
      case @compare.call(@key, new_key)
        when  1 then @left.add(new_key, new_val)
        when  0 then @val = new_val
        when -1 then @right.add(new_key, new_val)
      end
    end
  end

  def max_depth
    if self.empty?
      0
    else
      [@left.max_depth, @right.max_depth].max + 1
    end
  end
  
  def find search_key
    return nil if self.empty?
    case @compare.call(@key, search_key)
      when  1 then @left.find(search_key)
      when  0 then @val
      when -1 then @right.find(search_key)
    end
  end

  def to_s
    # branch, print key, val, left, right, recurse
    if self.branch?
      left  = (@left.key  == nil) ? '*' : @left.key
      right = (@right.key == nil) ? '*' : @right.key
      "(k: #{@key}, v: #{@val}, l: #{left}, r: #{right})\n" + @left.to_s + @right.to_s
    # leaf, print key, val
    elsif self.leaf?
      "(k: #{@key}, v: #{@val})\n"
    # empty
    else
      ""
    end
  end

  def print
    puts self.to_s
  end

end

num_compare = lambda { |n1,n2| n1 <=> n2 }

tree = Tree.new(num_compare)
tree.add(9, :a)
tree.add(4, :w)
tree.add(1, :t)
tree.add(6, :q)
assert_equal(3, tree.max_depth)
assert_equal(:a, tree.find(9))
assert_equal(:t, tree.find(1))
assert_equal("(k: 9, v: a, l: 4, r: *)\n(k: 4, v: w, l: 1, r: 6)\n(k: 1, v: t)\n(k: 6, v: q)\n", tree.to_s)
