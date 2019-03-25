{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Handler.Carta where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--Afrom From Entity Carta
cartaForm :: Maybe Carta -> AForm Handler Carta
cartaForm carta = Carta
                 <$> areq textField "Nombre de la Carta" (cartaNombrecarta <$> carta)
                 <*> areq (selectFieldList tipo) "Tipo de Carta" (cartaTipo <$> carta)
                 <*> areq (selectFieldList atributo) "Atributo" (cartaAtributo <$> carta)
                 <*> areq nivelField "Nivel " (cartaNivel <$> carta)
                 <*> areq textField "Descripcion" (cartaDescripcion <$> carta)
                 <*> areq ataqueField "Ataque " (cartaAtaque <$> carta)
                 <*> areq defensaField "Defensa" (cartaDefensa <$> carta)
                 <*> areq (selectFieldList icono) "Icono" (cartaIcono <$> carta)
               where
                 tipo :: [(Text,Text)]
                 tipo = [("Monstruo - Efecto", "Monstruo - Efecto"), ("Monstruo - Fusion", "Monstruo - Fusion"), ("Monstruo - Normal", "Monstruo - Normal"), ("Monstruo - Ritual", "Monstruo - Ritual"), ("Monstruo - Sincro", "Monstruo - Sincro"), ("Monstruo - XYZ", "Monstruo - XYZ"), ("Monstruo - Pendulo", "Monstruo - Link"), ("Magia - Normal", "Magia - Normal"), ("Magia - Ritual", "Magia - Ritual"), ("Magia - Equipo", "Magia - Equipo"), ("Magia - Continua", "Magia - Continua"), ("Magia - Campo", "Magia - Campo"), ("Magia - Juego Rapido", "Magia - Juego Rapido"), ("Trampa - Normal", "Trampa - Normal"), ("Trampa - Continua", "Trampa - Continua"), ("Trampa - ContraEfecto", "Trampa - contraEfecto")]
                 atributo :: [(Text, Text)]
                 atributo = [("Agua", "Agua"), ("Fuego", "Fuego"), ("Luz", "Luz"), ("Oscuridad", "Oscuridad"), ("Tierra", "Tierra"), ("Viento", "Viento")]
                 errorMessage :: Text
                 errorMessage = "No se puede realizar la accion"
                 nivelField = checkBool (<12) errorMessage intField
                 ataqueField = checkBool (<8000) errorMessage intField
                 defensaField = checkBool (<8000) errorMessage intField
                 icono :: [(Text, Text)]
                 icono = [("Agua", "水"), ("Fuego", "火事"), ("Luz", "軽い"), ("Oscuridad", "闇"), ("Tierra", "地球"), ("Viento", "風")]


--CRUD
--Create
getCartaNewR :: Handler Html
getCartaNewR = do
            (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cartaForm Nothing
            defaultLayout $ do
                let actionR = CartaNewR
                $(widgetFile "Carta")

postCartaNewR :: Handler Html
postCartaNewR = do
		((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ cartaForm  Nothing
		case result of
		     FormSuccess carta -> do
				 _ <- runDB $ insertEntity carta
				 redirect CartaListR
		     _ -> defaultLayout $ do
			let actionR = CartaNewR
			$(widgetFile "Carta")

--Delete
postCartaDeleteR :: CartaId -> Handler ()
postCartaDeleteR cartaId = do
                    runDB $ delete cartaId
                    redirect CartaListR

--list
getCartaListR ::  Handler Html
getCartaListR  = do
                cartas <- runDB $ getAllCartas
                ( _ , _ ) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cartaForm Nothing
                defaultLayout $ do
                   $(widgetFile "CartaView")


getAllCartas :: DB [Entity Carta]
getAllCartas = selectList [] [Asc CartaNombrecarta]

