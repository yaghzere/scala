package object Logique {

  // une interprétation associe une valeur de vérité à une proposition
  // Par exemple, on pourrait avoir l'interprétation ssuivante :
  // Map ( "A" -> true , "B" -> true, "C" -> false, "D" -> true)
  type Interpretation = Map[String, Boolean]

  // une base de connaissance est une liste d'expressions
  type BaseConnaissance = List[Expression]

  // un théorème est une expression
  type Theoreme = Expression

  // une expression logique se compose de constantes, de propositions,
  // et des opérateurs et, ou, non, ainsi que l'implication et l'équivalence
  sealed trait Expression

  sealed trait OperateurBinaire extends Expression {
    val expGauche: Expression
    val expDroite: Expression
  }

  case class Constante(valeur: Boolean) extends Expression

  case class Proposition(nom: String) extends Expression

  case class Non(exp: Expression) extends Expression

  case class Ou(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  case class Et(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  case class Implique(expGauche: Expression, expDroite: Expression) extends OperateurBinaire // (vrai => faux) est faux, tout le reste est vrai

  case class Equivalent(expGauche: Expression, expDroite: Expression) extends OperateurBinaire


  // Question 1 :
  // calculer la valeur de vérite d'une expression, étant donnée une interprétation
  def evaluation(expr: Expression)(implicit interp: Interpretation): Boolean = expr match {
    case Constante(valeur) => valeur
    case Proposition(nom) => interp(nom)
    case Non(exp) => !evaluation(exp)
    case Et(expGauche, expDroite) => evaluation(expGauche) && evaluation(expDroite)
    case Ou(expGauche, expDroite) => evaluation(expGauche) || evaluation(expDroite)
    case Implique(expGauche, expDroite) => !evaluation(expGauche) || evaluation(expDroite)
    case Equivalent(expGauche, expDroite) => evaluation(expGauche) == evaluation(expDroite)
  }

  // Question 2 :
  // renvoyer l'ensemble des noms des propositions comprises dans
  // un ensemble d'expression
  def ensembleProposition(expressions: Set[Expression]): Set[String] = expressions.flatMap {
    case Proposition(name) => Set(name)
    case Et(lhs, rhs) => ensembleProposition(Set(lhs, rhs))
    case Ou(lhs, rhs) => ensembleProposition(Set(lhs, rhs))
    case Non(e) => ensembleProposition(Set(e))
    case Implique(lhs, rhs) => ensembleProposition(Set(lhs, rhs))
    case Equivalent(lhs, rhs) => ensembleProposition(Set(lhs, rhs))
  }.toSet


  //Question 3 :
  // renvoyer la liste de toutes les interprétations possibles étant donnée
  // l'ensemble des propositions possibles dans l'ordre du code de Gray
  // exemple pour 3 propositions : FFF, FFV, FVV, FVF, VVF, VVV, VFV, VFF
  def listeInterpretation(e: Set[String]): List[Interpretation] = {
    val n = e.size
    val grayCode = (0 until (1 << n)).map(i => (i ^ (i >> 1)).toBinaryString.reverse.padTo(n, '0').reverse)
    val sortedPropositions = e.toList.sorted
    grayCode.map(code => sortedPropositions.zip(code.map(_ == '1')).toMap).toList
  }

  // question 4 :
  // définir si une expression est une tautologie, et si elle est consistante

  // tautologie = vrai pour tout interprétation
  def tautologie(expr: Expression): Boolean = ???

  // consistante = au moins un modèle
  def consistante(expr: Expression): Boolean = ???


  // Question 5 :
  // définir la fonction permettant d'afficher une expression
  def affichage(expr: Expression): String = ???


  // Le tableau de vérité
  class Tableau(val BC: BaseConnaissance, val Th: Theoreme) {

    override def toString: String =
      "Base de connaissances :\n" +
        BC.zipWithIndex.map { case (expr, index) => s"(${index + 1}) " + affichage(expr) }.mkString("\n") +
        "\nThéorème :\n" + affichage(Th)

    // un ligne du tableau de vérité
    class Ligne(val valeurInter: List[Boolean], val valeurBC: List[Boolean], val valeurTh: Boolean)

    // Question 6 :
    // définir les propositions ainsi que toutes les lignes du tableau de vérité
    val propositions: Set[String] = ???

    val lignes: List[Ligne] = ???

    //Question 7 :
    // Définir la fonction qui renvoie vrai si la base de connaissance infère le théorème
    def preuve: Boolean = ???


    //Question 8 :
    // définir une méthode qui affiche tout ou parti du tableau de vérité
    def toStringSelectif(f: Ligne => Boolean): String = ???

  }


  // Question 9
  // à partir d'une chaine de caractère, renvoyé, si possible, l'expression correspondante :
  def parseExpression(expr: String): Option[Expression] = ???

}







