defmodule HighSchoolSweetheart do
  def first_letter(name) do
    name |> String.trim() |> String.slice(0, 1)
  end

  def initial(name) do
    ( name |> first_letter() |> String.upcase() ) <> "." 
  end

  def initials(full_name) do
    [a, b] = full_name |> String.trim() |> String.split()
    "#{initial(a)} #{initial(b)}"
  end

  def pair(full_name1, full_name2) do
    i1 = initials(full_name1)
    i2 = initials(full_name2)
    """
         ******       ******
       **      **   **      **
     **         ** **         **
    **            *            **
    **                         **
    **     #{i1}  +  #{i2}     **
     **                       **
       **                   **
         **               **
           **           **
             **       **
               **   **
                 ***
                  *
    """

  end
end
