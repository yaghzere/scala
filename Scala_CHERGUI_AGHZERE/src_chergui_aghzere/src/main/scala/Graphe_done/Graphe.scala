package Graphe

import scala.annotation.tailrec
import scala.reflect.internal.util.NoPosition.{end, start}

case class Noeud(nom: String)

case class Arc(extremite1: String, extremite2: String)

case class Graphe(noeuds: Set[Noeud], arcs: Set[Arc]) {

  def +(arc: Arc): Graphe = {
    val new_graph = Graphe(noeuds +Noeud(arc.extremite1) + Noeud(arc.extremite2), arcs + arc)
    new_graph
  }

  def +(autre: Graphe): Graphe = {
   val new_graph = Graphe(this.noeuds ++ autre.noeuds, this.arcs ++ autre.arcs)
    new_graph
  }

  def voisins(noeud: Noeud): Set[Noeud] = {

   // Créez un Set de noeuds contenant les voisins du noeud en question
   val voisins = arcs.flatMap(arc => {
     if (arc.extremite1 == noeud.nom) Set(Noeud(arc.extremite2))
     else if (arc.extremite2 == noeud.nom) Set(Noeud(arc.extremite1))
     else Set.empty[Noeud]
   })
    // Excluez le noeud en question du Set de voisins
    voisins.diff(Set(noeud))
  }

  def degre(noeud: Noeud): Int = {
    voisins(noeud).size
  }

  def distance(depart: Noeud, arrive: Noeud): Option[Int] = {
    def recherche_voisin(current: Set[Noeud], vu: Set[Noeud]): Set[Noeud] = {
      current.flatMap(voisins) -- vu
    }

  def calcul_distance(current: Set[Noeud], vu: Set[Noeud], distance: Int): Option[Int] = {
      if (current(arrive)) Some(distance)
      else {
        val voisin: Set[Noeud] = recherche_voisin(current, vu)
        if (!voisin.isEmpty) {
          if (voisin(arrive)) Some(distance + 1)
          else calcul_distance(voisin, vu ++ current, distance + 1)
        } else None
      }
    }

    calcul_distance(Set(depart), Set(depart), 0)
  }


  lazy val composantesConnexes: Set[Set[Noeud]] = {
    def affectConnexe(node: Noeud, connexe: Set[Set[Noeud]]): Set[Set[Noeud]] = {
      if (connexe.nonEmpty) {
        if (distance(node, connexe.head.head).isDefined) {
          val header = connexe.head + node
          connexe.tail + header
        } else {
          affectConnexe(node, connexe.tail) + connexe.head
        }
      } else {
        connexe + Set(node)
      }
    }
    def recherche_composantes_connexes(nodes: Set[Noeud], connexe: Set[Set[Noeud]]): Set[Set[Noeud]] = {
      if (nodes.nonEmpty) {
        val new_connexe = affectConnexe(nodes.head, connexe)
        recherche_composantes_connexes(nodes.tail, new_connexe)
      } else connexe
    }

    recherche_composantes_connexes(noeuds.tail, Set(Set(noeuds.head)))
  }

  lazy val estBicoloriable: Boolean = {
    def contient_voisin(voisin: Set[Noeud], couleur: Set[Noeud]): Boolean = {
      if (couleur.nonEmpty) {
        if (voisin.nonEmpty) {
          if (couleur.contains(voisin.head)) true
          else contient_voisin(voisin.tail, couleur)
        } else false
      } else false
    }
    def spread(nodes: Set[Noeud], bleu: Set[Noeud], rouge: Set[Noeud], couleur: Int): Boolean = {
      val voisin = nodes.flatMap(voisins)
      if (nodes.nonEmpty) {
        if (couleur == 0) {
          if (contient_voisin(voisin, rouge)) false
          else spread(nodes.tail, bleu, rouge + nodes.head, (couleur + 1)%2)
        } else {
          if (contient_voisin(voisin, bleu)) false
          else spread(nodes.tail, bleu + nodes.head, rouge, (couleur + 1)%2)
        }
      } else true
    }
    def verifier_biocolorite(elements: Set[Set[Noeud]]): Boolean = {
      if (elements.nonEmpty) {
        spread(elements.head, Set(), Set(), 0) && verifier_biocolorite(elements.tail)
      } else true
    }
    verifier_biocolorite(composantesConnexes)
  }
}
