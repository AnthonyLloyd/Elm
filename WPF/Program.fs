module Lloyd.UI.WPFApp

open System
open System.Windows
open ExampleUI

[<EntryPoint;STAThread>]
let main argv =
    let window = new Window(Title="WindowsApp.WPF")
    let nativeUI = WPF.CreateNaiveUI window
    let app = CounterList.app
    UI.run nativeUI app
    Application().Run(window)
