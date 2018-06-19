module FSharp.CommandLine.Internals.Abstraction

type State<'Args, 'a> = 'Args -> ('a * 'Args)

type IStateConfig<'Config, 'Args, 'a> =
  abstract member Config: 'Config -> 'Config
  abstract member Func:   'Args -> ('a * 'Args)

[<Struct>]
type StateConfig<'Config, 'Args, 'a> = {
    config: 'Config -> 'Config
    func:   State<'Args, 'a>
  }
  with
    interface IStateConfig<'Config, 'Args, 'a> with
      member this.Config conf = this.config conf
      member this.Func args   = this.func args

module StateConfig =
  let inline scbind (f: 'a -> #IStateConfig<'Config, 'Args, 'b>) (g: 'Config -> 'Config) (m: #IStateConfig<'Config, 'Args, 'a>) =
    {
      config = m.Config >> g
      func = 
        fun args ->
          let (a, args) = m.Func args
          (f a).Func args
    }

  let inline returnValue (a: 'a) : StateConfig<_, _, 'a> =
    {
      config = id
      func = fun args -> (a, args)
    }

  let inline bind f m = m |> scbind f id
    
  let inline mapConfig g m = m |> scbind returnValue g
  
  let inline map f m = m |> bind (fun x -> returnValue (f x))
  
  let inline zip (m: #IStateConfig<_, _, 'a>) (n: #IStateConfig<_, _, 'b>) (f: 'a -> 'b -> 'c) =
    scbind (fun a ->
              {
                config = id
                func = fun args -> let (b, args) = n.Func args in (f a b, args)
              })
            n.Config
            m

  let inline combine (a: #IStateConfig<_,_,_>) (b: #IStateConfig<_,_,_>) = zip a b (fun _ b -> b) 

  let args =
    {
      config = id
      func = fun args -> (args, args)
    }
