/*package NReines

class NReines(val n: Int) {


  // Question 1 :
  // étant donnée la position des reines précedentes, renvoye vrai si la colone est compatible
  def estOk(col: Int, reines: List[Int]): Boolean = ???

  // Question 2
  // calcule la liste des solutions
  lazy val solutions: Set[List[Int]] = ???

  // Question 3
  // Retourne le nombre de solutions
  lazy val nombreSolutions: Int = ???

  // question 4
  // transforme une solution en un String afichable
  def afficheSolution(solution: List[Int]): String = ???

  def afficheToutesSolutions(): Unit = for {
    (solution, i) <- solutions.zipWithIndex
  } println(s"Solution N°${i + 1} :\n " + afficheSolution(solution))


}*/



package NReines

import Listes.contient

/*class NReines(val n: Int) {


  // Question 1 :
  // étant donnée la position des reines précedentes, renvoye vrai si la colone est compatible
  def estOk(col: Int, reines: List[Int]): Boolean = {
    val nb_reines = reines.size
    def estOkRec(liste_reines_rec: List[Int]) : Boolean ={
      val nb_reines_restantes = liste_reines_rec.size
      val diff = nb_reines +1 - nb_reines_restantes
      if (liste_reines_rec.isEmpty) true
      else if (liste_reines_rec.head == col) false
      else if (liste_reines_rec.head - diff == col ||liste_reines_rec.head + diff == col) false
      else estOkRec(liste_reines_rec.tail)
    }
    estOkRec(reines)
  }

  // Question 2
  // calcule la liste des solutions
  lazy val solutions: Set[List[Int]] = {

    def solution_partielle(rang : Int): Set[List[Int]]={
      if (rang == 1) (0 until n).map(i => List(i)).toSet
      else for{
        solution <- solution_partielle(rang-1)
        reine <- 0 until n
        if estOk(reine,solution)
      }yield reine::solution

    }
    solution_partielle(n)
  }

  // Question 3
  // Retourne le nombre de solutions
  lazy val nombreSolutions: Int = solutions.size

  // question 4
  // transforme une solution en un String afichable
  def afficheSolution(solution: List[Int]): String = {
    solution.map(i =>{
      "O ".repeat(i) + "X" + " O".repeat(n-i-1)
    }).mkString("\n")
  }

  def afficheToutesSolutions(): Unit = for {
    (solution, i) <- solutions.zipWithIndex
  } println(s"Solution N°${i + 1} :\n " + afficheSolution(solution))


}*/


class NReines(val n: Int) {

  // Question 1 :
  // étant donnée la position des reines précedentes, renvoye vrai si la colone est compatible
  def estOk(col: Int, reines: List[Int]): Boolean = {
    val nb_reines = reines.size
    def estOkRec(liste_reines_rec: List[Int]) : Boolean ={
      val nb_reines_restantes = liste_reines_rec.size
      val diff = nb_reines +1 - nb_reines_restantes
      if (liste_reines_rec.isEmpty) true
      else if (liste_reines_rec.head == col) false
      else if (liste_reines_rec.head - diff == col ||liste_reines_rec.head + diff == col) false
      else estOkRec(liste_reines_rec.tail)
    }
    estOkRec(reines)
  }

  // Question 2
  // calcule la liste des solutions
  lazy val solutions: Set[List[Int]] = {
    def solution_partielle(rang : Int): Set[List[Int]]={
      if (rang == 1) (0 until n).map(i => List(i)).toSet
      else for{
        solution <- solution_partielle(rang-1)
        reine <- 0 until n
        if estOk(reine,solution)
      }yield reine::solution
    }
    solution_partielle(n)
  }

  // Question 3
  // Retourne le nombre de solutions
  lazy val nombreSolutions: Int = solutions.size

  // question 4
  // transforme une solution en un String afichable
  def afficheSolution(solution: List[Int]): String = {
    solution.map(i => {
      "O ".repeat(i) + "X" + " O".repeat(n - i - 1)
    }).mkString("\n")
  }

  def afficheToutesSolutions(): Unit = {
    val solutionStrings = solutions.zipWithIndex.map {
      case (solution, i) => s"Solution N°${i + 1} :\n " + afficheSolution(solution)
    }
    solutionStrings.foreach(println)
  }

}

