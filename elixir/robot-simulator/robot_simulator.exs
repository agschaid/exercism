
defmodule RobotSimulator do

  defmodule Robot do
     @type t :: %__MODULE__{
      direction:   atom,
      position: {integer, integer}
     }

    @enforce_keys [:direction, :position]
    defstruct [:direction, :position]
  end

  @directions [:north, :east, :south, :west]

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: atom, position :: {integer, integer}) :: Robot.t()
  def create(direction \\ :north, position \\ {0,0})

  def create(direction, _) when direction not in @directions do
    {:error, "invalid direction"}
  end

  def create(direction, position={x, y}) when is_integer(x) and is_integer(y) do
    %Robot{direction: direction, position: position}
  end

  def create(_, _) do
    {:error, "invalid position"}
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: Robot.t(), instructions :: String.t()) :: Robot.t()
  def simulate(robot, "") do
    robot
  end

  def simulate(robot, "R" <> instructions) do
    robot |> turn(:right) |> simulate(instructions)
  end

  def simulate(robot, "L" <> instructions) do
    robot |> turn(:left) |> simulate(instructions)
  end

  def simulate(robot, "A" <> instructions) do

    {x1, y1} = robot.position

    {x2, y2} = case robot.direction do
      :north -> {0, 1}
      :east  -> {1, 0}
      :south -> {0, -1}
      :west  -> {-1, 0}
    end

    %{robot | position: {x1+x2, y1+y2} } |> simulate(instructions)
  end

  def simulate(_, _) do
    {:error, "invalid instruction"}
  end

  defguard is_a_rotation(rotation) when rotation in [:right, :left]

  # Turn the robot according to the given rotation.
  # Valid rotations are: `:right` and `:left`
  @spec turn(robot :: Robot.t(), rotation :: atom) :: Robot.t()
  defp turn(robot, rotation) when is_a_rotation(rotation) do

    directions = 
      if rotation == :right do
        @directions 
      else 
        Enum.reverse(@directions) 
      end

    directions ++ [Enum.at(directions, 0)]  # every direction has a successor
    |> Enum.drop_while(fn(x) -> robot.direction != x end)
    |> Enum.at(1)    # pick the one after the current robot direction
    |> (fn(new_direction) -> %{robot | direction: new_direction} end).()
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: Robot.t()) :: atom
  def direction(%{direction: dir}) do dir end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: Robot.t()) :: {integer, integer}
  def position(%{position: pos}) do pos end

end
