import scala.io.Source
import scala.io.StdIn.readLine

object FinalAssignmentHangman extends App {
  /**
   * This is an Assignment for Scala Class
   * Created by Mara and Antra
   */
  val srcPath = "./src/resources/dictionaryWords.txt"

  /**
   * Gets info from a text file
   *
   * @param srcPath - file name
   * @return lines or words to use for the game
   */
  def getLinesFromFile(srcPath: String): List[String] = {
    val bufferedSource = Source.fromFile(srcPath)
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    lines
  }

  var words = getLinesFromFile(srcPath)
  val firstLine = words.head
  /**
   * Gets random words from file
   *
   * @param lines - word to be used in the game
   *
   */
  def randomWord(lines: List[String]): String = {
    lines(scala.util.Random.nextInt(lines.length))
  }

  val randomWordForGuessing = randomWord(words)

  /**
   * Splits word into characters
   */

  def wordSplit(word: String): List[Char] = {
    word.toList
  }

  /**
   * Joins words with a space between
   *
   */
  def wordJoin(wordlist: List[Char]): String = {
    wordlist.mkString(" ")
  }

  val word = randomWordForGuessing.toUpperCase()
  val lengthOfWord = word.length
  //  println(word) //THE SECRET WORD
  val charCountWord = word.toList.distinct.size // count unique char in the secret word
  //  println(charCountWord)

  /**
   * Set only letters to be accepted
   *
   */

  def alphaSett: Set[Char] = {
    ('A' to 'Z').toSet
  }

  val myLoop = wordSplit(word)
  val alphaSet: Set[Char] = alphaSett
  /**
   * Drawing hangMan picture
   *
   * @param tries - in cases of loss, drawing piece by piece is coming up
   */

  def pictureFun(tries: Int): Unit = {
    tries match {
      case 0 => println("__\n|    |\n|    O\n|   /|\\\n|   / \\\n|")
      case 1 => println("__\n|    |\n|    O\n|   /|\\\n|   /\n|")
      case 2 => println("__\n|    |\n|    O\n|   /|\\\n|\n| ")
      case 3 => println("__\n|    |\n|    O\n|   /|\n|\n|")
      case 4 => println("__\n|    |\n|    O\n|   / \n|\n|")
      case 5 => println("__\n|    |\n|    O\n|   \n|\n|")
    }
  }
  /**
   * Main game loop
   */


  def mainGameLoop(): Unit = {
    var is_game_needed = true
    while (is_game_needed) {
      println("Welcome to the Game HangMan!")
      val response = readLine(" Do you want to play Y/N ?")
      if (response.toLowerCase.startsWith("y")) {
        play(word)
      }
      else is_game_needed = false
      println(s" It is end of the game, the secret word was $word ")
    }
  }
  /**
   * Running up play
   *
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
    println(s" Let's play Hangman!")
    println(s"The secret word has $lengthOfWord letters $wordShow")

    while (tries > 0 && !guessed && !exit) {
      val guess = readLine("Please guess a letter or a word").toUpperCase()
      if (guess.length == 1 && alphaSet.contains(guess.head)) {
        if (guessedLetters.contains(guess)) println(s"You have already tried this letter, $guess")
        else if (!word.contains(guess.head)) {
          println(s"$guess is not in the word.")
          tries -= 1
          pictureFun(tries)
          guessedLetters += guess
          println(s"You have only $tries guesses left")
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
          //          println(s" $result letter is number $mee in the word")
          //          println(s"Correctly guessed letters $guessedCorrect") // letters are in the order of guess order, not like they are in the secret word
          var setCorrectGuesses = guessedCorrect.toList
          var listCorrectChars = setCorrectGuesses.map(_.head)
          for (c <- word) {
            if (listCorrectChars.contains(c)) print (c)
            else print("_")
          }
          println()
          val ff = guessedCorrect.size
          if (ff == charCountWord) {
            println(s"You have guessed the secret word $word,Congratulations!")
            guessed = true
          }
        }
      }
      else if (guess.length == word.length) {
        if (guess == word) {
          println(s"You have guessed the word $guess, Congratulations!")
          guessed = true
        }
        else if (guess != word) {
          println(s"$guess is not the secret word")
          tries -= 1
          pictureFun(tries)
          guessedWords += guess
          println(s"You have left only $tries guesses")
        }
        else {
          println("not a valid guess")
          tries -= 1

          println(s"You have left only $tries guesses")
          pictureFun(tries)
          guessedWords += guess
        }
      }
      else {
        println("not a valid guess")
        tries -= 1
        println(s"You have left only $tries guesses")
        pictureFun(tries)
        guessedWords += guess

      }
    }


  }
  mainGameLoop()

}