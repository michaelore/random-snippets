#Try naturals(1).each {|x| puts x}
class LazyList
    include Enumerable
    attr_accessor :car
    attr_writer :cdr

    def initialize(first, rest=nil)
        @car = first
        @cdr = rest
    end

    def cons(first)
        LazyList.new(first, self)
    end

    def cons!(first)
        newself = self.clone
        @car = first
        @cdr = newself
        self
    end

    def self.lcons(first, &block)
        LazyList.new(first, block)
    end

    def cdr
        if @cdr.is_a?(Proc)
            @cdr = @cdr.call
        else
            @cdr
        end
    end

    def each(&block)
        node = self
        loop do
            block.call(node.car)
            if node.cdr
                node = node.cdr
            else
                break
            end
        end
    end

    def inspect
        acc = ''
        each {|x| acc << x.inspect << ' '}
        acc.chop!
        '(' + acc + ')'
    end
end

def naturals(n)
    LazyList.lcons(n) {naturals(n+1)}
end
