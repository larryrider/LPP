// Lawrence Rider García


// Practica 10



// Ejercicio 1.a

func sumaParejas(parejas: ArraySlice<(Int, Int)>) -> Int{
    if (!parejas.isEmpty){
        let primero = parejas[parejas.startIndex]
        let suma = primero.0 + primero.1
        return suma + sumaParejas(parejas[parejas.startIndex+1..<parejas.endIndex])
    } else{
        return 0
    }
}

let parejas = [(1, 1), (2, 2), (3, 3)]
print("Ejercicio 1.a:")
print("La suma de las parejas: \(parejas) es \(sumaParejas(ArraySlice(parejas)))")



// Ejercicio 1.b

func sumaParejasArray(parejas: ArraySlice<(Int, Int)>) -> [Int]{
    if (!parejas.isEmpty){
        let primero = parejas[parejas.startIndex]
        let suma = primero.0 + primero.1
        return [suma] + sumaParejasArray(parejas[parejas.startIndex+1..<parejas.endIndex])
    } else{
        return []
    }
}
print("Ejercicio 1.b:")
print(sumaParejasArray([(1, 1), (2, 2), (3, 3)]))



// Ejercicio 2

enum Coord{
    case X, Y, Z;
}


func aplica3D(array: ArraySlice<(Int, Int, Int)>, funcion f: (Int)->Int, coord c: Coord) -> [(Int, Int, Int)]{
    if(array.isEmpty){
        return []
    } else{
        let primero = array[array.startIndex]
        let resto = array[array.startIndex+1..<array.endIndex]
        var aux = [(0, 0, 0)]
        switch c{
            case .X:
                aux = [(f(primero.0), primero.1, primero.2)]
            case .Y:
                aux = [(primero.0, f(primero.1), primero.2)]
            case .Z:
                aux = [(primero.0, primero.1, f(primero.2))]
        }
        return aux + aplica3D(resto, funcion: f, coord: c)
    }
}

func suma2(x: Int) -> Int {
   return x + 2
}
print("Ejercicio 2:")
print(aplica3D([(1,2,3), (4,5,6), (7,8,9), (10,11,12)], funcion: suma2, coord: Coord.Y))



// Ejercicio 3

func compruebaParejas(array: ArraySlice<(Int)>, funcion f: (Int)->Int) -> [(Int, Int)] {
    if(array[array.startIndex+1..<array.endIndex].isEmpty){
        return []
    } else{
        let primero = array[array.startIndex]
        let segundo = array[array.startIndex+1]
        let resto = array[array.startIndex+1..<array.endIndex]

        if(f(primero) == segundo){
            return [(primero, segundo)] + compruebaParejas(resto,funcion: f)
        } else{
            return compruebaParejas(resto, funcion: f)
        }
    }
}

func cuadrado(x: Int) -> Int {
   return x * x
}
print("Ejercicio 3:")
print(compruebaParejas([2, 4, 16, 5, 10, 100, 105], funcion: cuadrado))



// Ejercicio 5

indirect enum BinaryTree{
    case Empty
    case Node(Int,BinaryTree,BinaryTree)
}

let arbol: BinaryTree = .Node(8, .Node(2, .Empty, .Empty), .Node(12, .Empty, .Empty))

func suma(abb: BinaryTree) -> Int{
    switch(abb){
        case .Empty:
            return 0
        case let .Node(raiz, iz, de):
            return raiz + suma(iz) + suma(de)
    }
}

print("Ejercicio 5")
print(suma(arbol))
