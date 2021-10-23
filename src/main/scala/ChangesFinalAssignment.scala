import scala.io.StdIn.readLine
import scala.io.Source

object ChangesFinalAssignment extends App  {
  /**
   * This is an Assignment for Scala Class
   * Created by Mara and Antra
   */
  val srcPath = "./src/resources/dictionaryWords.txt"

  def getLinesFromFile(srcPath: String): List[String] = {
    val bufferedSource = Source.fromFile(srcPath) //we have a source stream it could be network stream but here it is file stream
    val lines = bufferedSource.getLines.toList //so we transfer this stream until it ends into lines
    //lines we define the ending character newline or \n
    bufferedSource.close //very important to close a file after reading, do not leave it hanging!
    lines
  }
  var words = getLinesFromFile(srcPath)
  val firstLine = words.head

  /**
   * creating random word to play
   * @param lines - words from a file
   * @return
   */
  def randomWord(lines: List[String]): String = {
    lines(scala.util.Random.nextInt(lines.length))
  }

  val randomWordForGuessing = randomWord(words)

  // Split the word into individual letters.
  def wordSplit(word : String) : List[Char] = {
    word.toList
  }

  // Join the list of characters together with a space in-between.
  def wordJoin(wordlist : List[Char]) : String = {
    wordlist.mkString(" ")
  }

  val word = randomWordForGuessing.toUpperCase()
  val lengthOfWord = word.length

  /**
   * Setting only letters to be accepted
   *
   */

  def alphaSett: Set[Char] = {
    ('A' to 'Z').toSet
  }

  val alphaSet: Set[Char] = alphaSett

  /**
   * Drawing hangMan picture
   * @param tries - in cases of loss, drawing is coming up
   */

  def pictureFun (tries:Int): Unit = {
    tries match {
      case 0 => println("__\n|  |\n|  O\n| /|\\\n| / \\\n|")
      case 1 => println("__\n|  |\n|  O\n| /|\\\n| /")
      case 2 => println("__\n|  |\n|  O\n| /|\\\n| ")
      case 3 => println("__\n|  |\n|  O\n| /| ")
      case 4 => println("__\n|  |\n|  O\n| / ")
      case 5 => println("__\n|  |\n|  O\n|  ")
    }
  }


  //val playerName = readLine("Player what is your name?")
  //println(s"The secret word has $lengthOfWord letters")

  /**
   * Running up play
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
          pictureFun(tries)
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
            println(s"You have guessed all the letter in the word!")
            val guess2 = readLine("Please guess the word").toUpperCase()
            if (guess2 == word) {
              println(s"You guess the word $guess, Congratulations!")
              guessed = true
            }
            else if (guess2 != word) {
              println(s" $guess2 is not the word")
              tries -= 6
              guessedWords += guess
              println(s"You lost")
              println("__\n|  |\n|  O\n| /|\\\n| / \\\n|")
            }
          }
        }
      }
      else if (guess.length == word.length ) {
        if (guess == word) {
          println(s"You guess the word $guess, congratulations")
          guessed = true
        }
        else if (guess != word ) {
          println(s"$guess is not the secret word")
          tries -= 1
          pictureFun(tries)
          guessedWords += guess
          println(s"You have left only $tries guesses")
        }
        else  {
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

  /**
   * Full Game loop
   */

  def mainGameLoop(): Unit = {
    var is_game_needed = true
    while (is_game_needed) {
      val response = readLine("New game Y/N ?")
      if (response.toLowerCase.startsWith("y")) {
        play(word)
      }
      else is_game_needed = false
      println("Bye bye ")//in this case we end the game on anything other text starting with y or Y
    }

  }

  mainGameLoop()

}