package com.example.traverse

import com.example.util.MList
import com.example.util.MCons
import com.example.util.MNil

case class Person(name: String)
trait PersonService {
    def findPersonByName(name: String): Option[Person]
    def findPeopleByNames(names: MList[String]): Option[MList[Person]] = { ???
    //   names match {
    //       case MNil => None
    //       case MCons(h, t) => 
    //           findPersonByName(h) match {
    //               case None => None
    //               case Some(person) => Option(MCons(person, findPeopleByNames(t)))
    //           }
    //   }
        
    }
}
