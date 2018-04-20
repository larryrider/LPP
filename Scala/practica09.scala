// 
// Lawrence Rider Garcia	
//


//EJERCICIO 1

def resta1(x: Int) = x -1

def transforma(t: (Int,Int,Int), f:(Int)=>Int, c:String): (Int,Int,Int) =
  c match {
    case "x" => (f(t._1), t._2, t._3)
    case "y" => (t._1, f(t._2), t._3)
    case "z" => (t._1, t._2, f(t._3))
    }
    
def aplica3D(lista:List[(Int,Int,Int)], f:(Int)=>Int, c:String):List[(Int,Int,Int)] =
lista.map((x)=>transforma(x,f,c))

assert(aplica3D(List((1,1,1), (-3,-3,-3), (6,6,6)), resta1 _, "x") == List((0,1,1), (-4,-3,-3), (5,6,6)))
assert(aplica3D(List((1,1,1), (-3,-3,-3), (6,6,6)), resta1 _, "y") == List((1,0,1), (-3,-4,-3), (6,5,6)))
assert(aplica3D(List((1,1,1), (-3,-3,-3), (6,6,6)), resta1 _, "z") == List((1,1,0), (-3,-3,-4), (6,6,5)))



//EJERCICIO 2

def intercambia(lista: List[(Int,Int)]): List[(Int,Int)] = {
  if (lista.isEmpty) Nil 
  else (lista.head._2,lista.head._1) :: intercambia(lista.tail)
}

assert(intercambia(List((5,9), (20,48))) == List((9,5), (48,20)))
assert(intercambia(List((-5,30))) == List((30,-5)))


def compruebaParejas(lista: List[Int], f: (Int)=>Int) : List[(Int,Int)] = {
  if (lista.isEmpty) Nil
  else if (lista.tail.isEmpty) Nil
  else if (f(lista.head) == lista.tail.head) (lista.head,lista.tail.head) :: compruebaParejas(lista.tail,f)
  else compruebaParejas(lista.tail, f)
}

assert(compruebaParejas(List(3, 9, 81, 1, 1, 5, 23), (x)=>{x*x}) == List((3,9), (9,81), (1,1)))
assert(compruebaParejas(List(3, 6, 12, 1, 2, 5, 23), (x)=>{x+x}) == List((3,6), (6,12), (1,2)))
assert(compruebaParejas(List(3, 1, -1, 5, 2, 0, 23), (x)=>{x-2}) == List((3,1), (1,-1), (2,0)))
//EJERCICIO 3

def min[A](x: A, y: A,f:(A,A)=>Boolean): A = {
  if (f(x,y)) x
  else y
}

def minimo[A](lista: List[A], f:(A,A)=>Boolean): A = {
  if(lista.tail.isEmpty) lista.head
  else min(lista.head, minimo(lista.tail,f),f)
}


assert(minimo(List(1,3,3), (x: Int, y: Int) => {x < y}) == 1)
assert(minimo(List(1), (x: Int, y: Int) => {x < y}) == 1)
assert(minimo(List(-21,8,30), (x: Int, y: Int) => {x < y}) == -21)

assert(minimo(List("juan", "pepe", "adrian"), (x: String, y: String) => {x < y}) == "adrian")
assert(minimo(List("j"), (x: String, y: String) => {x < y}) == "j")
assert(minimo(List("d", "b", "ax"), (x: String, y: String) => {x < y}) == "ax")


def minTupla[A,B](t1: (A,B), t2: (A,B), f:((A,B))=> Int): (A,B) = {
  if(f(t1)<f(t2))t1
  else t2
}

def minimaTupla[A,B](lista: List[(A,B)], f:((A,B))=> Int): (A,B) = {
  if(lista.tail.isEmpty) lista.head
  else minTupla(lista.head,minimaTupla(lista.tail,f),f)
}

assert(minimaTupla(List(("buenas",4),("bien",35),("holaquetal",0)),(t: (String, Int)) => {t._1.length + t._2}) == ("holaquetal",0))
assert(minimaTupla(List(("holaquetal",0)),(t: (String, Int)) => {t._1.length + t._2}) == ("holaquetal",0))
assert(minimaTupla(List(("buenas",5),("comoestamos",1)),(t: (String, Int)) => {t._1.length + t._2}) == ("buenas",5))


//EJERCICIO 4


