@main def rucksack(): Unit =
  print(solution("""vJrwpWtwJgWrhcsFMMfFFhFp
                   |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                   |PmmdzqPrVvPwwTWBwg
                   |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                   |ttgJtRGJQctTZtZT
                   |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin))

def charToValue(c: Char): Int =
  if c.isUpper then
    Char.char2int(c.toLower) - (96 - 26)
  else
    Char.char2int(c) - 96


def findChar(s: String): Char =
  val (s1, s2) = s.splitAt(s.length/2)
  val x = s1.toSet.intersect(s2.toSet)
  x.head.toChar

def computeScore(s: String): Int =
  val score = charToValue(findChar(s))
  score

def solution(s: String): Int =
  s.split('\n').map(computeScore).sum