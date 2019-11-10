import React from 'react'
import { Label, RadioGroup, Radio } from '@blueprintjs/core'

const Security = ({ app, update }) => (
  <Label>
    <b>Application Type</b>&nbsp;<span className="bp3-text-muted">required</span>
    <RadioGroup 
      label=" "
      inline={true}
      selectedValue={app.security}
      onChange={(e) => update({ ...app, security: e.target.value }) }
    >
      <Radio label="Public" value="public" />
      <Radio label="Confidential" value="confidential" />
    </RadioGroup>
  </Label>
)

export default Security
