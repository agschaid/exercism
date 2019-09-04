
defmodule RobotSimulator do

  defmodule Robot do
     @type t :: %__MODULE__{
      direction:   atom,
      position: {integer, integer}
     }

    @enforce_keys [:direction, :position]
    defstruct [:direction, :position]
  end

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: atom, position :: {integer, integer}) :: Robot.t()
  def create(direction \\ nil, position \\ nil) do
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: Robot.t(), instructions :: String.t()) :: Robot.t()
  def simulate(robot, instructions) do
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: Robot.t()) :: atom
  def direction(robot) do
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: Robot.t()) :: {integer, integer}
  def position(robot) do
  end
end
