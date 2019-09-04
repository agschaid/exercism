
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
  def simulate(robot, instructions) do
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: Robot.t()) :: atom
  def direction(robot) do
    robot.direction
  end

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: Robot.t()) :: {integer, integer}
  def position(robot) do
    robot.position
  end
end
