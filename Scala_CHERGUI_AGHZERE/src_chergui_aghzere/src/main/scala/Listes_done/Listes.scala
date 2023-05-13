package object Listes {

  // définir une méthode qui renvoie le dernier élément d'une liste s'il existe
  // s'il n'existe pas, on génére une exception NoSuchElementException
  @throws[NoSuchElementException]
  def dernier[T](liste: List[T]): T = {
    if(!liste.isEmpty){
      if (liste.tail.isEmpty) liste.head else dernier(liste.tail)
    }
    else throw new NoSuchElementException("empty list")
  }

  // définir une méthode qui renvoie le k-ieme élément d'une liste, en commençant par 0, s'il existe
  // s'il n'existe pas, on génére une exception IndexOutOfBoundsException
  @throws[IndexOutOfBoundsException]
  def kieme[T](liste: List[T], k: Int): T = {
    if(liste.isEmpty){
      throw new IndexOutOfBoundsException()
    }
    else if (k == 0){
      liste.head
    }
    else{
      kieme(liste.tail, k - 1)
    }
  }

  // définir une méthode qui renvoie le nombre d'élément de la liste
  def taille[T](liste: List[T]): Int = {
    if(liste.isEmpty) 0 else 1 + taille(liste.tail)
  }

  // définir une méthode qui renvoie true si l'élément x est présent dans la liste
  def contient[T](liste: List[T], x: T): Boolean = {
    if(liste.isEmpty) {
      false
    }
    else if(liste.head == x){
      true
    }
    else{
      contient(liste.tail, x)
    }
  }

  // définir une méthode qui renvoie une nouvelle liste avec le k-ième élement supprimé
  // si k <= 0, on supprime le premier élément
  // si k>= taille, on renvoie la même liste
  def supprimerKieme[T](liste: List[T], k: Int): List[T] = {
    if(liste.isEmpty || k >= taille(liste)){
      liste
    }
    else if(k <= 0) {
      liste.tail
    }
    else{
      liste.head :: supprimerKieme(liste.tail, k - 1)
    }
  }

  // définir une méthode qui renvoie une nouvelle liste avec l'élément e en k-ième position
  // si k <= 0, on l'ajoute en premier élément
  // si k>= taille, on l'ajoute en dernier
  def ajouterKieme[T](liste: List[T], k: Int, e: T): List[T] = {
    supprimerKieme(liste, k)
    if(liste.isEmpty){
      e :: liste
    }
    else if (k <= 0){
      e :: liste
    }
    /*else if (k >= taille(liste)){
      liste
    }*/
    else {
      if(k == 1){
        liste.head :: e :: liste.tail
      }
      else {
        liste.head :: ajouterKieme(liste.tail, k - 1, e)
      }
    }
  }

  // définir une méthode qui renvoie true si les deux listes sont identiques
  def identique[T](liste1: List[T], liste2: List[T]): Boolean = {
    if(taille(liste1) != taille(liste2)){
      false
    }
    else if(taille(liste1) == 0){
      true
    }
    else if (liste1.head != liste2.head){
      false
    }
    else {
      identique(liste1.tail, liste2.tail)
    }
  }

  // définir une méthode qui renvoie une nouvelle liste en ne gardant que les éléments qui renvoie true au prédicat
  def filtrer[T](liste: List[T], predicat: T => Boolean): List[T] = {
    if(liste.isEmpty){
      liste
    }
    else if (!predicat(liste.head)){
      filtrer(liste.tail, predicat)
    }
    else {
      liste.head :: filtrer(liste.tail, predicat)
    }
  }

  // définir une méthode qui renvoie une nouvelle liste où les éléments sont les résultats de f
  def image[T, U](liste: List[T], f: T => U): List[U] = {
    if(liste.isEmpty){
      List()
    }
    else{
      f(liste.head) :: image(liste.tail, f)
    }
  }


}