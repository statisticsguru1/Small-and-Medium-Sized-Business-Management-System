library(R6)

# Define the User Class
User <- R6Class(
  "User",
  public = list(
    user_id = NULL,
    name = NULL,
    accounts = list(),
    
    initialize = function(user_id, name) {
      self$user_id <- user_id
      self$name <- name
    },
    
    add_account = function(account) {
      self$accounts[[length(self$accounts) + 1]] <- account
    },
    
    get_accounts = function() {
      return(self$accounts)
    }
  )
)


# Define the Base Account Class as a Subclass of User
Account <- R6Class(
  "Account",
  inherit = User,
  public = list(
    account_number = NULL,
    balance = 0,
    loan_accounts = list(),
    
    initialize = function(user, account_number = NA, balance = 0) {
      self$user_id <- user$user_id
      self$name <- user$name
      self$account_number <- account_number
      self$balance <- balance
      user$add_account(self)
    },
    
    add_loan_account = function(loan_account) {
      self$loan_accounts[[length(self$loan_accounts) + 1]] <- loan_account
    },
    
    deposit = function(amount) {
      cat("Depositing:", amount, "to Account Number:", self$account_number, "\n")
      
      # Pay off loans first
      for (loan in self$loan_accounts) {
        if (amount <= 0) break
        if (loan$outstanding_balance > 0) {
          payment <- min(amount, loan$outstanding_balance)
          loan$make_payment(payment)
          amount <- amount - payment
        }
      }
      
      # Add remaining balance to main account
      if (amount > 0) {
        self$balance <- self$balance + amount
        cat("Remaining deposited to main account. New Balance:", self$balance, "\n")
      } else {
        cat("All deposited funds were used to pay off loans. No remaining balance.\n")
      }
    },
    
    withdraw = function(amount) {
      # Check if there are any outstanding loans
      if (any(sapply(self$loan_accounts, function(loan) loan$outstanding_balance > 0))) {
        cat("Cannot withdraw, there are outstanding loan balances.\n")
        return()
      }
      
      if (amount > self$balance) {
        cat("Insufficient funds.\n")
      } else {
        self$balance <- self$balance - amount
        cat("Withdrew:", amount, "New Balance:", self$balance, "\n")
      }
    },
    
    get_balance = function() {
      return(self$balance)
    }
  )
)

# Define the LoanAccount Class
LoanAccount <- R6Class(
  "LoanAccount",
  inherit = Account,
  
  public = list(
    interest_rate = NULL,
    outstanding_balance = NULL,
    monthly_payment = NULL,
    main_account = NULL,
    
    initialize = function(user, main_account, loan_amount = 0, interest_rate = 0.05, months = 12) {
      super$initialize(user = user, account_number = paste0("Loan-", length(main_account$loan_accounts) + 1))
      self$interest_rate <- interest_rate
      self$outstanding_balance <- loan_amount
      self$main_account <- main_account
      self$monthly_payment <- self$calculate_monthly_payment(months)
      main_account$add_loan_account(self)
    },
    
    calculate_monthly_payment = function(months) {
      rate_per_month <- self$interest_rate / 12
      payment <- (self$outstanding_balance * rate_per_month) / (1 - (1 + rate_per_month)^-months)
      return(payment)
    },
    
    make_payment = function(amount) {
      self$outstanding_balance <- max(0, self$outstanding_balance - amount)
      cat("Payment made:", amount, "Outstanding Balance:", self$outstanding_balance, "\n")
    },
    
    add_interest = function() {
      interest <- self$outstanding_balance * self$interest_rate / 12
      self$outstanding_balance <- self$outstanding_balance + interest
      cat("Interest added:", interest, "New Outstanding Balance:", self$outstanding_balance, "\n")
    }
  )
)

# Define the FixedAccount Class
FixedAccount <- R6Class(
  "FixedAccount",
  inherit = Account,
  
  public = list(
    maturity_date = NULL,
    
    initialize = function(user, balance = 0, maturity_date = Sys.Date() + 365) {
      super$initialize(user = user, account_number = paste0("Fixed-", length(user$get_accounts()) + 1), balance = balance)
      self$maturity_date <- maturity_date
    },
    
    withdraw = function(amount) {
      if (Sys.Date() < self$maturity_date) {
        cat("Cannot withdraw before maturity date:", self$maturity_date, "\n")
      } else if (any(sapply(self$loan_accounts, function(loan) loan$outstanding_balance > 0))) {
        cat("Cannot withdraw, there are outstanding loan balances.\n")
      } else {
        super$withdraw(amount)
      }
    }
  )
)

##############################################################################################333333333

# Create a User
user1 <- User$new(user_id = "U001", name = "John Doe")

# Create a Main Account for the User
main_acc <- Account$new(user = user1, account_number = "A12345", balance = 5000)

# Create Loan Accounts linked to the Main Account
loan1 <- LoanAccount$new(user = user1, main_account = main_acc, loan_amount = 2000, interest_rate = 0.05, months = 12)
loan2 <- LoanAccount$new(user = user1, main_account = main_acc, loan_amount = 1500, interest_rate = 0.05, months = 12)

# Deposit Money in Main Account
main_acc$deposit(1000)

# Check Main Account Balance
cat("Main Account Balance after deposit:", main_acc$get_balance(), "\n")

# Attempt Withdrawal from Fixed Account
fixed_acc <- FixedAccount$new(user = user1, balance = 3000, maturity_date = Sys.Date() + 180)
fixed_acc$withdraw(500)  # Should not allow withdrawal since there are outstanding loans

# Pay off Loans Completely
main_acc$deposit(3000)

# Attempt Withdrawal from Fixed Account Again
fixed_acc$withdraw(500)  # Should allow withdrawal after loans are fully paid
