defmodule HighScore do

  @default_score 0

  def new(), do: %{}

  def add_player(scores, name, score \\ @default_score) do
    Map.put_new(scores, name, score)
  end

  def remove_player(scores, name) do
    Map.delete(scores, name)
  end

  def reset_score(scores, name) do
    Map.put(scores, name, @default_score)
  end

  def update_score(scores, name, additional_score) do
    Map.update(scores, name, @default_score + additional_score, fn score -> score + additional_score end)
  end

  def get_players(scores) do
    Map.keys(scores)
  end
end
