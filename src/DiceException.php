<?php

namespace Dice;

use RuntimeException;
use Throwable;

/**
 * Thrown if Dice can't instantiate a class
 */
class DiceException extends RuntimeException
{
    /**
     * @var string|null
     */
    protected $class;

    public function __construct(
        string $message      = '',
        ?string $class       = null,
        int $code            = 0,
        ?Throwable $previous = null
    ) {
        $this->class = $class;

        parent::__construct($message, $code, $previous);
    }

    /**
     * Get the name of the class Dice couldn't instantiate, if known
     */
    public function getClass(): ?string
    {
        return $this->class;
    }
}
