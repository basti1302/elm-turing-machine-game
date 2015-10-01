module ProgramView (Model, init, update, view) where

import Html


type alias Model = String


init : Model
init = "Program View"


update: Model -> Model
update model = model


view : Model -> List Html.Html
view model  =
  [ Html.text model ]
