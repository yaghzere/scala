package object Parentheses {

  // Écrire une fonction récursive qui indique si une phrase dispose de parenthèses bien construite
  // Premier version  sans pattern matchin : marche bien
  /*def equilibre(phrase: String): Boolean = {
    def equi(s: String, n: Int):Boolean= {
      if (s.isEmpty) {
        if (n == 0) true
        else false
      }
      else {
        if (s.head == '(') equi(s.tail, n + 1)
        else {
          if (s.head == ')'){
            if(n==0) false
            else equi(s.tail, n - 1)
          }
          else equi(s.tail, n)
        }
      }
    }
    equi(phrase,0)
  }
*/
  // deuxieme version avec pattern matchin
  def equilibre(phrase: String): Boolean = {
    //phrase = phrase.toList
    def equi(s: List[Char], n: Int): Boolean = s match {
      case Nil if n == 0 => true
      case Nil if n != 0 => false
      case x :: rest if x =='(' => equi(rest, n + 1)
      case x :: reste if x == ')' && n == 0 => false
      case x :: reste if x ==')' && n != 0 => equi(reste, n - 1)
      case _::reste => equi(reste, n)
    }
    equi(phrase.toList, 0)
  }


  // pareil, mais générique
  def equilibreGenerique(co: Char, cf: Char)(phrase: String): Boolean = {
    def equi(s: List[Char], n: Int,co: Char, cf: Char): Boolean = s match {
      case Nil if n == 0 => true
      case Nil if n != 0 => false
      case x :: rest if x == co => equi(rest, n + 1,co,cf)
      case x :: reste if x == cf && n == 0 => false
      case x :: reste if x == cf && n != 0 => equi(reste, n - 1,co,cf)
      case _::reste => equi(reste, n,co,cf)
    }
    equi(phrase.toList, 0,co,cf)
  }

  // utiliser la fonction générique pour définir la version xml avec < et > comme caractère ouvrant/fermant
  lazy val equilibreXml: String => Boolean = {
    equilibreGenerique('<','>')
  }

}
