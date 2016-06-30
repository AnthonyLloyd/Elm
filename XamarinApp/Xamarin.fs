module ExampleUI.Xamarin

open Xamarin.Forms

let CreateNaiveUI (window:ContentPage) =
    
    let rec createUI ui : View =
        match ui with
        |Text text ->
            let c = Label(Text=string text)
            upcast c
        |Input (text,event) ->
            let c = Entry(Text=string text)
            let event = !event
            c.TextChanged.Add(fun _ -> let t = c.Text
                                       async { !event t } |> Async.Start)
            upcast c
        |Button (text,event) ->
            let c = Button(Text=string text)
            let event = !event
            c.Clicked.Add(fun _ -> async { (!event)() } |> Async.Start)
            upcast c
        |Div (layout,list) ->
            let children = List.map createUI list
            let c = StackLayout(Orientation=
                                    match layout with
                                    |Vertical->StackOrientation.Vertical
                                    |Horizontal->StackOrientation.Horizontal)
            List.iter (c.Children.Add>>ignore) children
            upcast c

    let rec locatePanel loc : View Layout =
        match loc with
        |[] -> window.Content :?> _
        |i::xs -> (locatePanel xs).Children.Item i :?> _

    let uiUpdate u =
        match u with
        | InsertUI (loc,ui) ->
            match loc with
            |[] -> window.Content <- createUI ui
            |i::xs -> (locatePanel xs).Children.Insert(i,createUI ui)
        | UpdateUI (loc,ui) ->
            let element = match loc with
                          |[] -> window.Content :?> _
                          |i::xs -> (locatePanel xs).Children.Item i
            match ui with
            | Text text -> (element :?> Label).Text <- string text
            | Input (text,_) -> (element :?> Entry).Text <- string text
            | Button (text,_) -> (element :?> Button).Text <- string text
            | Div _ -> ()
        | ReplaceUI (loc,ui) ->
            match loc with
            |[] -> window.Content <- createUI ui
            |i::xs ->
                let c = (locatePanel xs).Children
                c.RemoveAt i
                c.Insert(i,createUI ui)
        | RemoveUI loc ->
            match loc with
            |[] -> ()
            |i::xs -> (locatePanel xs).Children.RemoveAt i
        | EventUI _ -> ()

    { new INativeUI with
        member __.Send list =
            Device.BeginInvokeOnMainThread (fun () -> List.iter uiUpdate list)
    }


