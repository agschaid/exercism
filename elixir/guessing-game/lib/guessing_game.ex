defmodule GuessingGame do
  def compare(secret_number, guess \\ :no_guess)
  def compare(x, x) do
    "Correct"
  end
  def compare(_secret_number, :no_guess) do
    "Make a guess"
  end
  def compare(secret_number, guess) when abs(secret_number-guess) == 1 do
    "So close"
  end
  def compare(secret_number, guess) when guess < secret_number do
    "Too low"
  end
  def compare(secret_number, guess) when guess > secret_number do
    "Too high"
  end
end
