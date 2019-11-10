import React from 'react'
import { Label, Classes } from '@blueprintjs/core'

const Endpoint = ({ app, update }) => (
  <Label>
    <b>Redirect Uri</b>&nbsp;<span className="bp3-text-muted">required</span>
    <input
      className={Classes.INPUT}
      type="input"
      required
      value={app.endpoint}
      onChange={(e) => update({ ...app, endpoint: e.target.value })}
    />
  </Label>
)

export default Endpoint
