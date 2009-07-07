#calculates the probabilities of all the possible sums of two die rolls
require 'monad'

die = Dist::uniform([1, 2, 3, 4, 5, 6])

r = die.bind do |x|
    die.bind do |y|
        Dist::wrap(x+y)
    end
end.merge

r.inspect
