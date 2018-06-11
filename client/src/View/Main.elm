-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module View.Main
    exposing
        ( Parameters
        , markHasAnnotation
        , pageLayout
        , updateAnnotationsWithImage
        , updateLayout
        , viewAll
        , viewConfig
        , viewImages
        , viewNothing
        )

import Annotation.Viewer as Viewer exposing (Viewer)
import Data.AnnotatedImage as AnnotatedImage exposing (AnnotatedImage, Annotations)
import Data.RawImage as RawImage exposing (RawImage)
import Data.Tool as Tool exposing (Tool)
import Element exposing (Element, paragraph, button)
import Element.Attributes as Attributes exposing (alignLeft, alignRight, fill, height, width, paddingTop, spacing)
import Html exposing (Html)
import Image exposing (Image)
import Packages.Device as Device exposing (Device)
import Packages.StaticTreeMap as StaticTreeMap exposing (StaticTreeMap)
import Packages.Zipper as Zipper exposing (Zipper)
import StyleSheet as Style exposing (ColorVariations, Style, ButtonState)
import View.ActionBar as ActionBar
import View.AnnotationsArea as AnnotationsArea
import View.ClassesSideBar as ClassesSideBar
import View.DatasetSideBar as DatasetSideBar


-- TYPES #############################################################


type alias Parameters msg =
    { device : Device
    , actionBar : ActionBar.Parameters msg
    , annotationsArea : AnnotationsArea.Parameters msg
    , selectClassMsg : Int -> msg
    , selectImageMsg : Int -> msg
    }



-- FUNCTIONS #########################################################


viewNothing : Parameters msg -> Html msg
viewNothing params =
    Element.layout Style.sheet <|
        ActionBar.emptyView params.actionBar


viewImages : Parameters msg -> Viewer -> Zipper RawImage -> Html msg
viewImages params viewer images =
    Element.layout Style.sheet <|
        Element.column Style.None
            [ Attributes.height fill ]
            [ ActionBar.viewImages params.actionBar

            --|> Element.below [ datasetRawSideBar params.selectImageMsg images ]
            , AnnotationsArea.viewImageOnly viewer (Zipper.getC images)
            ]


viewConfig : Parameters msg -> Zipper Tool -> { selected : Int, all : StaticTreeMap String } -> Html msg
viewConfig params tools classes =
    Element.layout Style.sheet <|
        Element.el Style.None
            [ Attributes.height fill ]
            (ActionBar.viewConfig params.actionBar tools
                |> Element.below [ classesSideBar params.selectClassMsg classes ]
            )



{--viewAll : Parameters msg -> Zipper Tool -> Viewer -> { selected : Int, all : StaticTreeMap String } -> Zipper AnnotatedImage -> Html msg
viewAll params tools viewer ({ selected, all } as classes) annotatedImages =
    Element.layout Style.sheet <|
        Element.column Style.None
            [ Attributes.height fill ]
            [ ActionBar.viewAll params.actionBar tools
                |> Element.below [ exampleText2 ]
            , AnnotationsArea.view params.annotationsArea viewer (Zipper.getC annotatedImages)
            ] --}


viewAll : Parameters msg -> Zipper Tool -> Viewer -> { selected : Int, all : StaticTreeMap String } -> Zipper AnnotatedImage -> Html msg
viewAll params tools viewer ({ selected, all } as classes) annotatedImages =
    Element.layout Style.sheet <|
        Element.column Style.None
            [ Attributes.height fill ]
            [ ActionBar.viewAll params.actionBar tools
            , Element.row Style.None
                [ Attributes.width fill, Attributes.height fill ]
                [ instructionText
                , AnnotationsArea.view params.annotationsArea viewer (Zipper.getC annotatedImages)
                ]
            ]


instructionText =
    Element.paragraph (Style.Instruction Style.Paragraph)
        [ width (Attributes.percent 25), paddingTop 10, spacing 10, Attributes.yScrollbar ]
        [ Element.column
            Style.None
            [ Attributes.center ]
            [ Element.el (Style.Instruction Style.Title) [] (Element.text "INSTRUCTIONS\n")
            , Element.text "\n Please outline the objects in the images. \n To do so, select the outline tool and \n press on the image where you want to \n start outlining.\n Continue pressing while outlining until \n you're done. \n \n"
            , Element.newTab "https://google.fr" (Element.el (Style.Instruction Style.Link) [] (Element.text "Good outlines examples\n"))
            , Element.newTab "https://google.fr" (Element.el (Style.Instruction Style.Link) [] (Element.text "Bad outlines examples\n"))
            , Element.el (Style.Instruction Style.Title) [] (Element.text "\n GUIDELINES\n")
            , Element.text "\n 1. The whole object has to be inside the\n outline. \n 2. The outline must follow roughly the \n shape of the image\n 3. If there are more than one objects,\n outline only one \n."
            , Element.newTab "https://google.fr" (Element.el (Style.Instruction Style.Link) [] (Element.text "Video Tutorial \n"))
            ]
        ]



-- sub views helpers


datasetRawSideBar : (Int -> msg) -> Zipper RawImage -> Element Style var msg
datasetRawSideBar selectImageMsg images =
    DatasetSideBar.viewRaw selectImageMsg images
        |> Element.el Style.ClassesSidebar [ alignRight, paddingTop 10 ]


datasetAnnotatedSideBar : (Int -> msg) -> Zipper AnnotatedImage -> Element Style var msg
datasetAnnotatedSideBar selectImageMsg images =
    DatasetSideBar.viewAnnotated selectImageMsg images
        |> Element.el Style.ClassesSidebar [ alignRight, paddingTop 10 ]


classesSideBar : (Int -> msg) -> { selected : Int, all : StaticTreeMap String } -> Element Style var msg
classesSideBar selectClassMsg classes =
    ClassesSideBar.view selectClassMsg classes
        |> Element.el Style.ClassesSidebar [ alignLeft, paddingTop 10 ]



-- update helpers


markHasAnnotation : Bool -> Parameters msg -> Parameters msg
markHasAnnotation hasAnnotations ({ actionBar } as params) =
    if actionBar.hasAnnotations == hasAnnotations then
        params
    else
        { params | actionBar = { actionBar | hasAnnotations = hasAnnotations } }


pageLayout : Device -> { actionBarSize : ( Float, Float ), viewerSize : ( Float, Float ) }
pageLayout device =
    let
        ( barWidth, barHeight ) =
            ( device.size.width |> toFloat
            , ActionBar.responsiveHeight device |> toFloat
            )

        ( viewerWidth, viewerHeight ) =
            ( (device.size.width |> toFloat) * 0.75
              --ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
            , max 0 (toFloat device.size.height - barHeight)
            )
    in
        { actionBarSize = ( barWidth, barHeight )
        , viewerSize = ( viewerWidth, viewerHeight )
        }


updateLayout : Device.Size -> Parameters msg -> ( Parameters msg, ( Float, Float ) )
updateLayout size params =
    let
        device =
            Device.classify size

        layout =
            pageLayout device

        updateSize newSize parameters =
            { parameters | size = newSize }

        actionBar =
            updateSize layout.actionBarSize params.actionBar

        annotationsArea =
            updateSize layout.viewerSize params.annotationsArea
    in
        ( { params | device = device, actionBar = actionBar, annotationsArea = annotationsArea }
        , layout.viewerSize
        )


updateAnnotationsWithImage : Float -> Image -> Int -> Zipper { toolId : Int, annotations : Annotations } -> Parameters msg -> Parameters msg
updateAnnotationsWithImage zoom image selectedClassId annotations ({ annotationsArea } as params) =
    let
        newAnnotationsWithImage =
            AnnotationsArea.annotationsWithImage zoom image selectedClassId annotations
    in
        { params | annotationsArea = { annotationsArea | annotationsWithImage = Just newAnnotationsWithImage } }
