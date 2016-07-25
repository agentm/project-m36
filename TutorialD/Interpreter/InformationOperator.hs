{-# LANGUAGE CPP #-}
module TutorialD.Interpreter.InformationOperator where
import Data.Text
import Text.Parsec
import Text.Parsec.String
import TutorialD.Interpreter.Base

-- this module provides information about the current interpreter

data InformationOperator = HelpOperator |
                           GetVersionOperator 
                           
infoOpP :: Parser InformationOperator                           
infoOpP = helpOpP <|> getVersionP
  
helpOpP :: Parser InformationOperator          
helpOpP = reserved ":help" >> pure HelpOperator

getVersionP :: Parser InformationOperator
getVersionP = reserved ":version" >> pure GetVersionOperator

evalInformationOperator :: InformationOperator -> Either Text Text
evalInformationOperator GetVersionOperator = Right ("tutd " `append` PROJECTM36_VERSION)
-- display generic help
evalInformationOperator HelpOperator = Right $ intercalate "\n" help
  where
    help = ["tutd Help", 
            "Quick Examples:",
            ":showexpr true",
            ":showexpr relation{name Text, address Text}{tuple{name \"Steve\", address \"Main St.\"}}",
            "address := relation{tuple{name \"Steve\", address \"Main St.\"}}",
            ":showexpr true join false = false",
            "Relational Operators:",
            ":showexpr relation{a Int, b Text}{} -- relation creation",
            ":showexpr relation{tuple{c t}} -- relation creation",
            ":showexpr relation{tuple{a 4, b 4}}{a} -- projection",
            ":showexpr relation{tuple{a 5}} rename {a as num} -- rename",
            ":showexpr relation{tuple{d 10}} where d=10 or d=5 -- restriction",
            ":showexpr relation{tuple{d 10}} : {e:=add(@d,5)} -- extension",
            "Database Context Operators:",
            "animal := relation{tuple{name \"octopus\", legs_count 8}} -- assignment",
            "insert animal relation{tuple{name \"cat\", legs_count 4}} -- insertion",
            "car :: {model Text, make Text, year Int} -- definition",
            "undefine car -- undefine",
            "delete animal where legs_count=4 -- deletion",
            "update animal where name=\"octopus\" (name:=\"Mr. Octopus\") -- updating",
            "employee:=relation{id Int, name Text, age Int}{}; key emp_unique_id {id} employee --uniqueness constraint",
            "constraint age_gt_zero (employee{age} where ^lt(@age,0)){} equals false -- constraint",
            "notify teenager_added employee where ^lt(@age,20) and ^gte(@age,13) employee{age} where ^lt(@age,20) and ^gte(@age,13) -- change notification",
            "Graph Operators: ",
            ":jumphead <head_name> - change the current database context to point to a current head",
            ":jump <transaction_id> - change the current database context to that of a past transaction",
            ":commit - push the current context into the current head and make it immutable",
            ":rollback - discard any changes made in the current context",
            ":showgraph - display the transaction graph",
            "View more documentation at: https://github.com/agentm/project-m36/blob/master/docs/tutd_tutorial.markdown"
            ]
