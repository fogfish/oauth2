import React from 'react'
import { Label, Classes } from '@blueprintjs/core'

export const SecretKey = () => (
  <Label>
    Password&nbsp;<span className="bp3-text-muted">required</span>
    <input
      className={Classes.INPUT}
      id="secret"
      name="secret"
      type="password"
      required
    />
  </Label>
)
