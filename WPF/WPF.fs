module ExampleUI.WPF

open ExampleUI
open System.Windows
open System.Windows.Controls

let CreateNaiveUI (root:ContentControl) =
    
    let rec createUI ui : UIElement =
        match ui with
        |Text text ->
            let c = Label(Content=string text)
            upcast c
        |Input (text,event) ->
            let c = TextBox(Text=string text)
            let event = !event
            c.TextChanged.Add(fun _ -> let t = c.Text in async { !event t } |> Async.Start)
            upcast c
        |Button (text,event) ->
            let c = Button(Content=string text)
            let event = !event
            c.Click.Add(fun _ -> async { (!event)() } |> Async.Start)
            upcast c
        |Div (layout,list) ->
            let children = List.map createUI list
            let c = StackPanel(Orientation=match layout with |Vertical->Orientation.Vertical |Horizontal->Orientation.Horizontal)
            List.iter (c.Children.Add>>ignore) children
            upcast c

    let updateUI ui (element:UIElement) =
        match ui with
        | Text text -> (element :?> Label).Content <- string text
        | Input (text,_) -> (element :?> TextBox).Text <- string text
        | Button (text,_) -> (element :?> Button).Content <- string text
        | Div _ -> ()

    let rec locatePanel loc : Panel =
        match loc with
        |[] -> root.Content :?> _
        |i::xs -> (locatePanel xs).Children.Item i :?> _

    let uiUpdate u =
        match u with
        | InsertUI (loc,ui) ->
            match loc with
            |[] -> root.Content <- createUI ui
            |i::xs -> (locatePanel xs).Children.Insert(i,createUI ui)
        | UpdateUI (loc,ui) ->
            let uiElement = match loc with |[] -> root.Content :?> _ |i::xs -> (locatePanel xs).Children.Item i
            updateUI ui uiElement
        | ReplaceUI (loc,ui) ->
            match loc with
            |[] -> root.Content <- createUI ui
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
            root.Dispatcher.Invoke (fun () -> List.iter uiUpdate list)
    }


