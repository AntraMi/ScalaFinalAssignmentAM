import scala.io.StdIn.readLine

object FinalAssignmentHangman extends App  {
  /**
   * This is an Assignment for Scala Class
   * Created by Mara and Antra
   */

  val word = "car".toUpperCase()
  val lengthOfWord = word.length

  /**
   * Setting only letters to be accepted
   *
   */
  // Set of upper case letters.
  def alphaSett: Set[Char] = {
    ('A' to 'Z').toSet
  }

  val alphaSet: Set[Char] = alphaSett


  //val playerName = readLine("Player what is your name?")
  //println(s"The secret word has $lengthOfWord letters")

  /**
   * Game loop
   * @param word - word to be played
   */

  def play(word: String): Unit = {
    val wordShow = "_" * word.length

    val me = word.toList
    var guessed = false
    val exit = false
    val guessedLetters = scala.collection.mutable.ListBuffer.empty[String]
    val guessedWords = scala.collection.mutable.ListBuffer.empty[String]
    val guessedCorrect = scala.collection.mutable.ListBuffer.empty[String]
    var tries = 6
    println("Let's play Hangman!")
    println(s"The secret word has $lengthOfWord letters $wordShow")

    while (tries > 0 && !guessed && !exit) {
      val guess = readLine("Please guess a letter or a word").toUpperCase()
      if (guess.length == 1 && alphaSet.contains(guess.head)) {
        if (guessedLetters.contains(guess)) println(s"You already guess this letter, $guess")
        else if (!word.contains(guess.head)) {
          println(s"$guess is not in the word.")
          tries -= 1
          guessedLetters += guess
          println(s"You have left only $tries guesses")
        }
        else {
          println(s"Good job, $guess is in the word")
          guessedCorrect += guess
          guessedLetters += guess
          val result = for {
            c <- word
            if c == guess.head
          } yield c.toUpper
          val mee = word.indexOf(result) + 1
          println(s" $result letter is number $mee in the word")
          println(s"Correctly guessed letters $guessedCorrect") // letters are in the order of guess order, not like they are in the secret word
          var ff = guessedCorrect.size
          if (ff == word.length) {
            println(s"You have guessed all the letters in the word!")
            val guess2 = readLine("Please guess the word").toUpperCase()
            if (guess2 == word) {
              println(s"You have guessed the word $guess, Congratulations!")
              guessed = true
            }
            else if (guess2 != word) {
              println(s" $guess2 is not the word")
              tries -= 6
              guessedWords += guess
              println(s"You lost")
            }
          }
        }
      }
      else if (guess.length == word.length ) {
        if (guess == word) {
          println(s"You have guessed the word $guess. Congratulations!")
          guessed = true
        }
        else if (guess != word ) {
          println(s"$guess is not the secret word")
          tries -= 1
          guessedWords += guess
          println(s"You have only $tries guesses left")
        }
        else  {
          println("not a valid guess")
          tries -= 1
          println(s"You have only $tries guesses left")
          guessedWords += guess
        }
      }
      else {
        println("not a valid guess")
        tries -= 1
        println(s"You have only $tries guesses left")
        guessedWords += guess

      }
    }
  }

  play(word)

  val afterGame = readLine("Do you want new game?").toUpperCase()


}