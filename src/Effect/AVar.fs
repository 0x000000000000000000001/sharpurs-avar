// FFI for Effect.AVar: a faithful port of the queue based implementation of
// purescript-avar. Callbacks are `Either Error a -> Effect Unit`; the FFIUtil
// record provides the constructors (left, right, nothing, just, killed,
// filled, empty) as boxed functions.

type private AvarEntry =
    { mutable Canceled: bool
      Callback: obj }

type private AvarPut =
    { mutable Canceled: bool
      Value: obj
      Callback: obj }

type private AvarState =
    { Gate: obj
      mutable Value: obj
      mutable HasValue: bool
      mutable Error: obj
      mutable Draining: bool
      Takes: System.Collections.Generic.List<AvarEntry>
      Reads: System.Collections.Generic.List<AvarEntry>
      Puts: System.Collections.Generic.List<AvarPut> }

let private newAvarState (value: obj) (hasValue: bool) =
    { Gate = obj ()
      Value = value
      HasValue = hasValue
      Error = null
      Draining = false
      Takes = System.Collections.Generic.List<AvarEntry>()
      Reads = System.Collections.Generic.List<AvarEntry>()
      Puts = System.Collections.Generic.List<AvarPut>() }

let private utilField (util: obj) (name: string) : obj =
    Map.find name (unbox<Map<string, obj>> util)

let private utilCall (util: obj) (name: string) (arg: obj) : obj =
    sharpurs_apply (utilField util name) arg

let private runEffect (eff: obj) =
    if not (isNull eff) then
        (unbox<obj -> obj> eff) null |> ignore

let private takeEntry (queue: System.Collections.Generic.List<AvarEntry>) : AvarEntry option =
    let mutable index = 0
    let mutable found = None
    while found.IsNone && index < queue.Count do
        let entry = queue.[index]
        queue.RemoveAt index
        if entry.Canceled then () else found <- Some entry
    found

let private takePut (queue: System.Collections.Generic.List<AvarPut>) : AvarPut option =
    let mutable index = 0
    let mutable found = None
    while found.IsNone && index < queue.Count do
        let entry = queue.[index]
        queue.RemoveAt index
        if entry.Canceled then () else found <- Some entry
    found

// Mirror of purescript-avar's drainVar: every state transition happens under
// the gate, the callbacks themselves run outside of it. The JavaScript
// implementation relies on the single-threaded event loop to never preempt a
// drain; native fibers run on several threads, so the drain must re-check for
// work under the gate before releasing it.
let private hasPendingWork (state: AvarState) : bool =
    if not (isNull state.Error) then
        state.Puts.Count > 0 || state.Takes.Count > 0 || state.Reads.Count > 0
    elif state.HasValue then
        state.Takes.Count > 0 || state.Reads.Count > 0
    else
        state.Puts.Count > 0

let private drainVar (util: obj) (state: AvarState) =
    let mutable work = true
    while work do
        let mutable claimed = false
        lock state.Gate (fun () ->
            if not state.Draining then
                state.Draining <- true
                claimed <- true)
        if not claimed then
            work <- false
        else
            try
                let mutable go = true
                while go do
                    let pending = System.Collections.Generic.List<unit -> unit>()
                    lock state.Gate (fun () ->
                        if not (isNull state.Error) then
                            let failure = utilCall util "left" state.Error
                            while state.Puts.Count > 0 do
                                match takePut state.Puts with
                                | Some entry -> let callback = entry.Callback in pending.Add(fun () -> runEffect (sharpurs_apply callback failure))
                                | None -> ()
                            while state.Takes.Count > 0 do
                                match takeEntry state.Takes with
                                | Some entry -> let callback = entry.Callback in pending.Add(fun () -> runEffect (sharpurs_apply callback failure))
                                | None -> ()
                            while state.Reads.Count > 0 do
                                match takeEntry state.Reads with
                                | Some entry -> let callback = entry.Callback in pending.Add(fun () -> runEffect (sharpurs_apply callback failure))
                                | None -> ()
                        else
                            let mutable put = None
                            if not state.HasValue then
                                put <- takePut state.Puts
                                match put with
                                | Some entry ->
                                    state.Value <- entry.Value
                                    state.HasValue <- true
                                | None -> ()
                            if state.HasValue then
                                let readsBefore = state.Reads.Count
                                let take = takeEntry state.Takes
                                let success = utilCall util "right" state.Value
                                for _ in 1 .. readsBefore do
                                    match takeEntry state.Reads with
                                    | Some entry -> let callback = entry.Callback in pending.Add(fun () -> runEffect (sharpurs_apply callback success))
                                    | None -> ()
                                match take with
                                | Some entry ->
                                    let callback = entry.Callback
                                    state.Value <- null
                                    state.HasValue <- false
                                    pending.Add(fun () -> runEffect (sharpurs_apply callback success))
                                | None -> ()
                            match put with
                            | Some entry ->
                                let callback = entry.Callback
                                let unitValue = utilCall util "right" null
                                pending.Add(fun () -> runEffect (sharpurs_apply callback unitValue))
                            | None -> ())
                    for action in pending do action ()
                    lock state.Gate (fun () ->
                        if (not state.HasValue && state.Puts.Count = 0)
                           || (state.HasValue && state.Takes.Count = 0 && state.Reads.Count = 0) then
                            go <- false)
            finally
                lock state.Gate (fun () ->
                    state.Draining <- false
                    work <- hasPendingWork state)

let empty = box (fun (_: obj) -> box (newAvarState null false))

let _newVar (value: obj) = box (fun (_: obj) -> box (newAvarState value true))

let _killVar (util: obj) (error: obj) (avar: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        lock state.Gate (fun () ->
            if isNull state.Error then
                state.Error <- error
                state.Value <- null
                state.HasValue <- false)
        drainVar util state
        box null)

let _putVar (util: obj) (value: obj) (avar: obj) (callback: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        let entry = { Canceled = false; Value = value; Callback = callback }
        lock state.Gate (fun () -> state.Puts.Add entry)
        drainVar util state
        box (fun (_: obj) ->
            lock state.Gate (fun () -> entry.Canceled <- true)
            box null))

let _takeVar (util: obj) (avar: obj) (callback: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        let entry = { Canceled = false; Callback = callback }
        lock state.Gate (fun () -> state.Takes.Add entry)
        drainVar util state
        box (fun (_: obj) ->
            lock state.Gate (fun () -> entry.Canceled <- true)
            box null))

let _readVar (util: obj) (avar: obj) (callback: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        let entry = { Canceled = false; Callback = callback }
        lock state.Gate (fun () -> state.Reads.Add entry)
        drainVar util state
        box (fun (_: obj) ->
            lock state.Gate (fun () -> entry.Canceled <- true)
            box null))

let _tryPutVar (util: obj) (value: obj) (avar: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        let mutable accepted = false
        lock state.Gate (fun () ->
            if not state.HasValue && isNull state.Error then
                state.Value <- value
                state.HasValue <- true
                accepted <- true)
        if accepted then drainVar util state
        box accepted)

let _tryTakeVar (util: obj) (avar: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        let mutable taken = None
        lock state.Gate (fun () ->
            if state.HasValue then
                taken <- Some state.Value
                state.Value <- null
                state.HasValue <- false)
        match taken with
        | Some value ->
            drainVar util state
            utilCall util "just" value
        | None -> utilField util "nothing")

let _tryReadVar (util: obj) (avar: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        let mutable current = None
        lock state.Gate (fun () -> if state.HasValue then current <- Some state.Value)
        match current with
        | Some value -> utilCall util "just" value
        | None -> utilField util "nothing")

let _status (util: obj) (avar: obj) =
    box (fun (_: obj) ->
        let state = unbox<AvarState> avar
        if not (isNull state.Error) then utilCall util "killed" state.Error
        elif state.HasValue then utilCall util "filled" state.Value
        else utilField util "empty")
