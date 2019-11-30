import React from 'react'
import { Label, Classes } from '@blueprintjs/core'

const AccessKey = () => (
  <Label>
    Email&nbsp;
    <span className="bp3-text-muted">required</span>
    <input
      className={Classes.INPUT}
      id="access"
      name="access"
      type="email"
      required
    />
  </Label>
)

export default AccessKey
