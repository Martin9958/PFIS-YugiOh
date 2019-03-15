 {-# LANGUAGE NoImplicitPrelude #-}
 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE TemplateHaskell #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE TypeFamilies #-}
 module Handler.Demo where

 import Import
 import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

 --Aform From Entity Demo
demoForm :: Maybe Demo -> AForm Handler Demo
demoForm   demo = Demo 
		<$> areq textField "fieldone" (demoFieldOne <$> demo)
		<*> areq intField "fieldTwo" (demoFieldTwo <$> demo) 
		<*> areq boolField "fieldThree" (demoFieldThree <$> demo) 


