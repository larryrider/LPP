// 
// Lawrence Rider Garcia	
//


//EJERCICIO 1
def ordenada(lista: List[Int]): Boolean = {
	var ordenado = true;
	if (lista.tail != Nil){
		if (lista.head < siguienteLista(lista))
			ordenada(lista.tail)
		else
			ordenado = false
	}

	ordenado
}

def siguienteLista(lista: List[Int]): Int = {

	lista.tail.head
}

// pruebas
assert(ordenada(List(1,-8,3)) == false)
assert(ordenada(List(1,2,5,23)) == true)


//EJERCICIO 2
def englobados(int1: (Int, Int), int2: (Int, Int)): Boolean ={
    if (((int1._1 <= int2._1) && (int2._2 <= int1._2)) || ( (int2._1 <= int1._1) && (int1._2 <= int2._2)))
        true
    else
    	false
}

def max (int1: Int, int2: Int): Int = {
	if (int1 < int2)
		int2
	else
		int1
}

def min (int1: Int, int2: Int): Int = {
	if (int1 < int2)
		int1
	else
		int2
}

def intersectar (int1: (Int, Int), int2: (Int, Int)): (Int, Int) ={
    if ((int2._1 <= int1._2) && (int1._1 <= int2._2)){
    	((max(int1._1,int2._1)), (min (int1._2,int2._2)))
    }
    else{
    	null
    }
}


// pruebas
assert(englobados((2,4),(6,7))==false)
assert(englobados((2,8),(6,7))==true)
assert(intersectar((2,4),(6,7))==null)
assert(intersectar((2,5),(3,7))==(3,5))


//EJERCICIO 3
def cuadradoLista(lista: List[Int]): List[Int]={

	if (lista.isEmpty)
		Nil
	else
		(lista.head*lista.head) :: cuadradoLista(lista.tail)
}


//pruebas
assert(cuadradoLista(List(1,2,3))==List(1,4,9))
assert(cuadradoLista(List(8,5,10))==List(64,25,100))


//EJERCICIO 4

def contienePatron(frase: String, patron: String): List[String] = {

	buscarPatron(frase.split(" +").toList,patron)

}

def buscarPatron(palabras: List[String], patron: String): List[String] = {


	if (palabras.tail != Nil){

		if (palabras.head.find(patron)){
			palabras.head :: buscarPatron(palabras.tail, patron)
		}
		else{
			buscarPatron(palabras.tail, patron)
		}
	}

}	
