namespace XamarinApp

open ExampleUI
open Xamarin.Forms

type App() =
    inherit Application()
    do
        let c = ContentPage()
        let nativeUI = Xamarin.CreateNaiveUI c
        let app = CounterList.app
        UI.run nativeUI app
        base.MainPage <- c