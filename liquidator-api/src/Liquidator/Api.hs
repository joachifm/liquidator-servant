{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator.Api
  ( Api
  , api
  , apiVersion

  , LiquidatorApi
  , liquidatorApi
  , swaggerDoc

  , module Liquidator.Schema
  ) where

import Control.Lens hiding (Strict)

import Data.Swagger
import Servant
import Servant.Swagger

import Liquidator.Schema

------------------------------------------------------------------------
-- User
------------------------------------------------------------------------

type GetUserById
  =  QueryParam' '[Required, Strict] "id" Int64
  :> Get '[JSON] User

type CreateUser
  =  ReqBody '[JSON] UserCreate
  :> Post '[JSON] User

type UpdateUser
  =  ReqBody '[JSON] User
  :> Put '[JSON] User

type DeleteUser
  =  ReqBody '[JSON] Text
  :> Delete '[JSON] NoContent

type GetUserByEmail
  =  QueryParam' '[Required, Strict] "email" Text
  :> Get '[JSON] User

type LoginUser
  =  ReqBody '[JSON] LoginData
  :> Post '[JSON] LoginSuccess

type RefreshToken
  =  ReqBody '[JSON] Refresh
  :> Post '[JSON] RefreshResult

type UserApi
  =  "user"
  :> (    GetUserById
     :<|> CreateUser
     :<|> UpdateUser
     :<|> DeleteUser
     :<|> ( "byEmail" :> GetUserByEmail )
     :<|> ( "login" :> LoginUser )
     :<|> ( "refresh" :> RefreshToken )
     )

------------------------------------------------------------------------
-- Recurring
------------------------------------------------------------------------

type GetRecurringTransaction
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "id" Int64
  :> Get '[JSON] RecurringTransaction

type AddRecurringTransaction
  =  ReqBody '[JSON] RecurringTransaction
  :> Post '[JSON] RecurringTransaction

type UpdateRecurringTransaction
  =  ReqBody '[JSON] RecurringTransaction
  :> Put '[JSON] RecurringTransaction

type DeleteRecurringTransaction
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "id" Int64
  :> Delete '[JSON] NoContent

-- TODO(joachifm) result should be pagination
type GetAllRecurringTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [RecurringTransaction]

-- TODO(joachifm) result should be pagination
type GetActiveRecurringTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [RecurringTransaction]

-- TODO(joachifm) result should be pagination
type GetRecurringTransactionsByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date_param" Text
  :> Get '[JSON] [RecurringTransaction]

-- TODO(joachifm) result should be pagination
type GetRecurringTransactionsByDateRange
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "start_date" Text
  :> QueryParam' '[Required, Strict] "end_date" Text
  :> Get '[JSON] [RecurringTransaction]

type RecurringApi
  =  "recurring"
  :> (    GetRecurringTransaction
     :<|> AddRecurringTransaction
     :<|> UpdateRecurringTransaction
     :<|> DeleteRecurringTransaction
     :<|> ( "all" :> GetAllRecurringTransactions )
     :<|> ( "active" :> GetActiveRecurringTransactions )
     :<|> ( "byDate" :> GetRecurringTransactionsByDate )
     :<|> ( "byDateRange" :> GetRecurringTransactionsByDateRange )
     )

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

type GetCompanyById
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] Company

type AddCompany
  =  ReqBody '[JSON] Company
  :> Post '[JSON] Company

type UpdateCompany
  =  ReqBody '[JSON] Company
  :> Put '[JSON] Company

type DeleteCompany
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Delete '[JSON] NoContent

type CompanyAddUser
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "user_id" Int64
  :> QueryParam' '[Optional, Strict] "user_role" Role
  :> Post '[JSON] NoContent

type CompanyRemoveUser
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "user_id" Int64
  :> Post '[JSON] NoContent

type CompanySetUserRole
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "user_id" Int64
  :> QueryParam' '[Required, Strict] "user_role" Role
  :> Post '[JSON] NoContent

type CompanyApi
  =  "company"
  :> (    GetCompanyById
     :<|> AddCompany
     :<|> UpdateCompany
     :<|> DeleteCompany
     :<|> ( "addUser" :> CompanyAddUser )
     :<|> ( "removeUser" :> CompanyRemoveUser )
     :<|> ( "setRole" :> CompanySetUserRole )
     )

------------------------------------------------------------------------
-- Balance
------------------------------------------------------------------------

type GetBalanceByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date_param" Text
  :> Get '[JSON] Balance

-- TODO(joachifm) produces pagination
type GetBalanceByDateRange
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "start_date" Text
  :> QueryParam' '[Required, Strict] "end_date" Text
  :> Get '[JSON] [Balance]

type GetBankBalance
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "id" Int64
  :> Get '[JSON] BankBalance

type CreateBankBalance
  =  ReqBody '[JSON] BankBalance
  :> Post '[JSON] BankBalance

type UpdateBankBalance
  =  ReqBody '[JSON] BankBalance
  :> Put '[JSON] BankBalance

type DeleteBankBalance
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "id" Int64
  :> Delete '[JSON] NoContent

type GetBankBalanceByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date_param" Text
  :> Get '[JSON] [BankBalance]

type GetBankBalanceByDateRange
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "start_date" Text
  :> QueryParam' '[Required, Strict] "end_date" Text
  :> Get '[JSON] [BankBalance]

type BalanceApi
  = "balance"
  :> (      GetBalanceByDate
       :<|> GetBalanceByDateRange
       :<|> ( "bank" :> (      GetBankBalance
                          :<|> CreateBankBalance
                          :<|> UpdateBankBalance
                          :<|> DeleteBankBalance
                          :<|> ( "byDate" :> GetBankBalanceByDate )
                          :<|> ( "byDateRange" :> GetBankBalanceByDateRange )
                        )
            )
     )

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

type GetTransactionById
  =  QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] Transaction

type AddTransaction
  =  ReqBody '[JSON] Transaction
  :> Post '[JSON] Transaction

type UpdateTransaction
  =  ReqBody '[JSON] Transaction
  :> Put '[JSON] Transaction

type DeleteTransaction
  =  QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Delete '[JSON] NoContent

type InsertTransactionArray
  =  ReqBody '[JSON] [Transaction]
  :> Post '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetAllTransaction
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetTransactionsByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date_param" Text
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetTransactionsByDateRange
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "start_date" Text
  :> QueryParam' '[Required, Strict] "end_date" Text
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetAllIncomeTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetAllExpenseTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [Transaction]

type TransactionApi
  = "transaction"
  :> (      GetTransactionById
       :<|> AddTransaction
       :<|> UpdateTransaction
       :<|> DeleteTransaction
       :<|> ( "insertArray" :> InsertTransactionArray )
       :<|> ( "all" :> GetAllTransaction )
       :<|> ( "byDate" :> GetTransactionsByDate )
       :<|> ( "byDateRange" :> GetTransactionsByDateRange )
       :<|> ( "income" :> "all" :> GetAllIncomeTransactions )
       :<|> ( "expense" :> "all" :> GetAllExpenseTransactions )
     )

------------------------------------------------------------------------
-- Liquidator
------------------------------------------------------------------------

type LiquidatorApi
  = "api"
  :> "v1"
  :> (     TransactionApi
      :<|> BalanceApi
      :<|> CompanyApi
      :<|> RecurringApi
      :<|> UserApi
     )

liquidatorApi :: Proxy LiquidatorApi
liquidatorApi = Proxy

------------------------------------------------------------------------
-- Toplevel
------------------------------------------------------------------------

type Api = SwaggerApi :<|> LiquidatorApi

api :: Proxy Api
api = Proxy
