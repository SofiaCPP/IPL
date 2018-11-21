@Decorator()
class A {
    public a?: string;
    public b?: A;
    public func =  (): string => {
        return this.a;
    }
}

// comment  
const x = "hello" + " " + "world" + 1;

{
    const b = 3;
}