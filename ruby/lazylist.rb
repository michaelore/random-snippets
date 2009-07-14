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
        block.call(@car)
        cdr.each(&block)
    end
end

def naturals(n)
    LazyList.lcons(n) {naturals(n+1)}
end
