#An exploration of monads in Ruby.
#The Perhaps stuff was inspired by this:
#http://www.randomhacks.net/articles/2007/02/21/refactoring-probability-distributions

require 'rational'

class Rational
    def inspect
        "#{@numerator}/#{@denominator}"
    end
end

module Monad
    def bind(&b)
        fmap(&b).mjoin
    end

    def skip(&b)
        bind {|x| b.call}
    end

    def self.included(base)
        def base.guard(bool)
            bool ? self.wrap(nil) : self.mzero
        end
    end
end

class Array
    include Monad

    def mjoin
        inject([]) {|combined, arr| combined + arr}
    end

    def fmap(&b)
        map(&b)
    end

    def Array::wrap(x)
        [x]
    end

    def mzero
        []
    end

    def mplus(x)
        + x
    end
end

class Perhaps
    attr_reader :value, :prob
    include Monad

    def initialize(x, p)
        @value = x
        @prob = p
    end

    def inspect
        "Perhaps " + @value.inspect + " " + @prob.inspect
    end

    def mjoin
        Perhaps::new(@value.value, @prob * @value.prob)
    end

    def fmap(&b)
        Perhaps::new(b.call(@value), @prob)
    end

    def Perhaps::wrap(x)
        Perhaps::new(x, 1)
    end
end

module PerhapsT
    def self.included(base)
        base.send :alias_method, :old_fmap, :fmap
        def fmap(&b)
            self.class.lift(old_fmap {|x| x.fmap(&b)})
        end
        def mjoin
            self.class.lift(old_fmap do |outP|
                outP.value.old_fmap do |inP|
                    Perhaps::new(inP, outP.prob).mjoin
                end
            end.mjoin)
        end
        def base.wrap(x)
            lift(superclass.wrap(Perhaps::wrap(x)))
        end
    end
end

class Dist < Array
    include PerhapsT

    def self.lift(arr)
        Dist::new(arr)
    end

    def self.uniform(arr)
        prob = Rational(1, arr.length)
        Dist::new(arr.map {|x| Perhaps::new(x, prob)})
    end

    def self.mzero
        lift([])
    end

    def merge
        acc = []
        each_index do |x|
            merged = false
            acc.each_index do |y|
                if self[x].value == acc[y].value
                    acc[y] = Perhaps::new(acc[y].value, acc[y].prob + self[x].prob)
                    merged = true
                end
            end
            if (not merged)
                acc.push(self[x])
            end
        end
        acc
    end
end
