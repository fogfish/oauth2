import React from 'react'
import { Label, Classes } from '@blueprintjs/core'

export const AccessKey = () => (
  <Label>
    Email&nbsp;<span className="bp3-text-muted">required</span>
    <input
      className={Classes.INPUT}
      id="access"
      type="email"
      autoFocus
      required
    />
  </Label>
)
