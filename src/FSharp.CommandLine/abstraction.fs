namespace FSharp.CommandLine.Internals

module Abstraction =
  type State<'Args, 'a> = 'Args -> ('a * 'Args)

  type StateConfig<'Config, 'Args, 'a> = {
      config: 'Config -> 'Config
      func:   State<'Args, 'a>
    }

  module StateConfig =
    let inline scbind (f: 'a -> StateConfig<'Config, 'Args, 'b>) (g: 'Config -> 'Config) (m: StateConfig<'Config, 'Args, 'a>) =
      {
        config = m.config >> g
        func = 
          fun args ->
            let (a, args) = m.func args
            (f a).func args
      }

    let inline returnValue (a: 'a) : StateConfig<_, _, 'a> =
      {
        config = id
        func = fun args -> (a, args)
      }

    let inline returnWith (f: unit -> 'a) : StateConfig<_, _, 'a> =
      {
        config = id
        func = fun args -> (f(), args)
      }

    let inline bind f m = m |> scbind f id
      
    let inline mapConfig g m = m |> scbind returnValue g
    
    let inline map f m = m |> bind (f >> returnValue)
    
    let inline zip (m: StateConfig<_, _, 'a>) (n: StateConfig<_, _, 'b>) (f: 'a -> 'b -> 'c) =
      scbind (fun a ->
          {
            config = id
            func = fun args -> let (b, args) = n.func args in (f a b, args)
          })
        n.config
        m

    let inline combine (a: StateConfig<_,_,_>) (b: StateConfig<_,_,_>) = zip a b (fun _ b -> b) 

    let args =
      {
        config = id
        func = fun args -> (args, args)
      }
