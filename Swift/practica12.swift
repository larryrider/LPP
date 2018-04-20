// Lawrence Rider García


// Practica 12



// Ejercicio 1

import Glibc

func rand(n: Int) -> Int {
    return random() % n
}

enum MarcaCoche: Int {
    case Mercedes=0, Ferrari, RedBull, McLaren
    
    static func random() -> MarcaCoche {
        let maxValue = McLaren.rawValue;
        
        let r = rand(maxValue+1)
        return MarcaCoche(rawValue: r)!
    }

}

enum TipoCambio: Int{
    case Automatico=0, Manual
    
    static func random() -> TipoCambio{
        let maxValue = Manual.rawValue
        
        let r = rand(maxValue+1)
        return TipoCambio(rawValue: r)!
    }
}

class Coche{
    var velocidadActual = 0.0
    var marcha = 0
    var distanciaRecorrida = 0.0
    var marca = MarcaCoche.random()
    
    var descripcion: String{
        get{
            switch(marca){
                case .Mercedes:
                    return "Mercedes"
                case .Ferrari:
                    return "Ferrari"
                case .RedBull:
                    return "RedBull"
                case .McLaren:
                    return "McLaren"
            }
        }
    }
    
    static let velocidadMaxima = 150.0
    static let marchaMaxima = 6
}

class CocheAutomatico: Coche{
    override var descripcion: String{
        get{
            switch(marca){
                case .Mercedes:
                    return "Mercedes Automático"
                case .Ferrari:
                    return "Ferrari Automático"
                case .RedBull:
                    return "RedBull Automático"
                case .McLaren:
                    return "McLaren Automático"
            }
        }
    }
    override var velocidadActual: Double{
        didSet{
            marcha = min(Int(velocidadActual / 25.0) + 1, 5)
            print("\(descripcion) viajando a \(velocidadActual) por hora con la marcha \(marcha)")
            distanciaRecorrida += velocidadActual
        }
    }
}

class CocheManual: Coche{
    override var descripcion: String{
        get{
            switch(marca){
                case .Mercedes:
                    return "Mercedes Manual"
                case .Ferrari:
                    return "Ferrari Manual"
                case .RedBull:
                    return "RedBull Manual"
                case .McLaren:
                    return "McLaren Manual"
            }
        }
    }
    
    override var marcha: Int{
        didSet{
            velocidadActual = 25.0 * Double(marcha)
        }
    }
    
    override var velocidadActual: Double{
        didSet{
            print("\(descripcion) viajando a \(velocidadActual) kilometros por hora con la marcha \(marcha)")
            distanciaRecorrida += velocidadActual
        }
    }
}

/*var prueba = CocheManual()
prueba.marcha = 1

var prueba2 = CocheAutomatico()
prueba2.velocidadActual = 141*/

class Carrera{
    var numCoches = 0
    var numHoras = 0
    
    var arrayCoches = [Coche]()
    
    init(numCoches: Int, horas numHoras: Int){
        self.numCoches = numCoches
        self.numHoras = numHoras
        for _ in 0..<numCoches {
            switch(TipoCambio.random()){
                case .Automatico:
                    arrayCoches.append(CocheAutomatico())
                case .Manual:
                    arrayCoches.append(CocheManual())
            }
        }
    }
    
    func descripcion(){
        print("\(numCoches) coches con una duracion de \(numHoras) horas")
        for coche in arrayCoches{
            print(coche.descripcion)
        }
    }
    
    func empezar(){
        for i in 1..<numHoras+1{
            print("\n\nHoras transcurridas \(i)")
            for coche in arrayCoches{
                if(coche is CocheAutomatico){
                    coche.velocidadActual = Double(rand(Int(Coche.velocidadMaxima) + 1))
                } else{
                    coche.marcha = rand(Coche.marchaMaxima + 1)
                }
            }
        }
    }
    
    func clasificacion(){
        
        var aux = arrayCoches.sort({$0.distanciaRecorrida>$1.distanciaRecorrida})
        for i in 1..<aux.count+1{
            let coche = aux[i-1]
            print("\(i) . \(coche.descripcion) (\(coche.distanciaRecorrida) kilometros recorridos)")
        }
    }
    
}

let carrera = Carrera(numCoches: 2, horas: 3)
print("\nDescripción de la carrera:")
carrera.descripcion()
print("\n!!! Comienza la carrera !!!")
carrera.empezar()
print("\n!!! Clasificación !!!")
carrera.clasificacion()





// Ejercicio 2

struct Punto {
    var x = 0.0, y = 0.0
}

struct Tamaño {
    var ancho = 0.0, alto = 0.0
}

class Rectangulo{
    var origen = Punto()
    var tamaño = Tamaño()
    
    var centro: Punto{
        get{
            let centroX = origen.x + (tamaño.ancho / 2)
            let centroY = origen.y + (tamaño.alto / 2)
            return Punto(x: centroX, y: centroY)
        }
        
        set{
            origen.x = newValue.x - (tamaño.ancho / 2)
            origen.y = newValue.y - (tamaño.alto / 2)
        }
    }
    
    var area: Double{
        return tamaño.ancho * tamaño.alto
    }
    
    init(origen: Punto, tamaño: Tamaño){
        self.origen = origen
        self.tamaño = tamaño
    }
}

class Circulo{
    var centro = Punto()
    var radio = 0.0
    let pi = 3.141592
    
    init(centro: Punto, radio: Double){
        self.centro = centro
        self.radio = radio
    }
    
    var area: Double{
        get{
            return radio * radio * pi
        }
        
        set{
            radio = sqrt(newValue/pi)
        }
    }
}

class Triangulo{
    var p1 = Punto()
    var p2 = Punto()
    var p3 = Punto()
    
    var area: Double{
        get{
            return abs(p1.x*(p2.y-p3.y)+p2.x*(p3.y-p1.y)+p3.x*(p1.y-p2.y))/2.0
        }
    }
    
    var centro: Punto{
        get{
            let x = (p1.x + p2.x + p3.x)/3
            let y = (p1.y + p2.y + p3.y)/3
            return Punto(x: x, y: y)
        }
        
        set{
            
        }
    }
    
}
