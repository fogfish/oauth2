import React from 'react'
import { Label, Classes } from '@blueprintjs/core'

const Identity = ({ app, update }) => (
  <Label>
    <b>Application</b>
    &nbsp;
    <span className="bp3-text-muted">required</span>
    <input
      className={Classes.INPUT}
      type="input"
      required
      defaultValue={app.app}
      onChange={e => update({ ...app, app: e.target.value })}
    />
  </Label>
)

export default Identity
