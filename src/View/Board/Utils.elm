module View.Board.Utils exposing (..)


boardBldgNo : number
boardBldgNo =
    3


boardBldgWidth : number
boardBldgWidth =
    220


boardBldgSpacing : number
boardBldgSpacing =
    10


boardWidth : number
boardWidth =
    boardBldgWidth * boardBldgNo + boardBldgSpacing * boardBldgNo
