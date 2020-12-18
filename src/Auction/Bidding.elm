module Auction.Bidding exposing (..)

import Morphir.SDK.StatefulApp exposing (StatefulApp(..))



{- Type aliases for modeling in the language of the business -}


type alias ID =
    String


type alias ProductID =
    String


type alias Price =
    Float


type alias Quantity =
    Int



{- Identifies a structure that can be associated to a persistance entity -}


type alias Bid =
    { product : ProductID
    , price : Price
    , quantity : Quantity
    }



{- These define the requests that can be made of this service -}


type BidCommand
    = PlaceBid ProductID Price Quantity
    | CancelBid ProductID



{- These define the responses that would result from requests -}


type BidEvent
    = BidPlaced ProductID Price Quantity
    | BidCanceled ProductID
    | InvalidQuantity Quantity
    | InvalidPrice Price


{- Defines that this is a stateful application that uses ID as the entity key (for possible partioning),
   accepts requests of type BidCommand,
   manages data in the form of a Bid,
   and produces events of type BidEvent.

   Note that there's no indication of whether the API is synchronous or asynchronous.  That's up to the implementation to decide.
-}
{- Defines the business logic of this app.
   That is whether or not to accept a request to open or close a Bid.
-}


logic : Maybe Bid -> BidCommand -> ( Maybe Bid, BidEvent )
logic state command =
    case command of
        PlaceBid productId price quantity ->
            if not (validPrice price) then
                ( state, InvalidPrice price )

            else if not (validQuantity quantity) then
                ( state, InvalidQuantity quantity )

            else
                ( Just (Bid productId price quantity)
                , BidPlaced productId price quantity
                )

        CancelBid p ->
            ( state, BidCanceled p)

validPrice : Price -> Bool
validPrice price =
    price >= 0

validQuantity : Quantity -> Bool
validQuantity quantity =
    quantity > 0


app : StatefulApp ID BidCommand Bid BidEvent
app =
    StatefulApp logic
